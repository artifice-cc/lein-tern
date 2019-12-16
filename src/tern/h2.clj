(ns tern.h2
  (:require [tern.db           :refer :all]
            [tern.log          :as log]
            [clojure.java.jdbc :as jdbc]
            [clojure.string    :as s])
  (:import [java.util Date]
           [java.sql PreparedStatement Timestamp]))

(declare foreign-key-exists? get-matching-foreign-keys index-exists? column-exists? table-exists? get-column-definition not-indexable-type?)

(def ^:dynamic *db* nil)
(def ^:dynamic *plan* nil)

(def ^{:doc "Set of supported commands. Used in `generate-sql` dispatch."
       :private true}
  supported-commands
  #{:create-table :drop-table :alter-table :create-index :drop-index :insert-into :update})

(defn ^:private remove-column-length
  "Sanitize a column name that might include a length, as these are not supported in H2"
  [column-name]
  (let [[_ name length] (re-matches #"(\w+)(:?\(\d+\))?" column-name)]
    (if (and name length) name column-name)))

(defn ^:private unsupported-column-spec
  [spec]
  (let [s (cond (string? spec)
                spec
                (keyword? spec)
                (name spec)
                (symbol? spec)
                (name spec)
                :else (str spec))]
    (or (re-matches #"(?i)CHARACTER SET .+" s)
        (re-matches #"(?i)COLLATE .+" s))))

(def ^:private column-spec-map
  {"DEFAULT NULL" "NULL"})

(defn ^:private remove-unsupported-column-specs
  [specs]
  (for [spec specs :when (not (unsupported-column-spec spec))]
    (get column-spec-map spec spec)))
   
(defn generate-pk
  [{:keys [primary-key] :as command}]
  (when primary-key
    (format "PRIMARY KEY (%s)" (to-sql-list primary-key))))

(defn generate-constraints
  [{:keys [constraints] :as command}]
  (let [fks 
        (when constraints
          (for [[constraint & specs] constraints]
            (do
              (format "CONSTRAINT %s FOREIGN KEY %s"
                      (to-sql-name constraint)
                      (s/join " " specs)))))]
    (when fks (map (fn [fk] [fk]) fks))))


(defn generate-options
  [table-options]
  (when (not-empty table-options)
    (s/join ", "
            (for [option table-options]
              (format "%s=%s" (:name option) (:value option))))))

(defmulti generate-sql
  (fn [c] (some supported-commands (keys c))))

(defmethod generate-sql
  :create-table
  [{table :create-table columns :columns table-options :table-options :as command}]
  (log/info " - Creating table" (log/highlight (name table)))
  (let [constraints (generate-constraints command)
        table-spec (generate-options table-options)
        pk (generate-pk command)]
    (if (and *db*
             (table-exists? *db* (to-sql-name table))
             (not (some (fn [prior] (= prior {:drop-table table})) @*plan*)))
      (do (log/info (format "  * Skipping CREATE TABLE %s because it already exists." (to-sql-name table)))
          nil)
      (if (not-empty table-spec)
        (concat
         (apply conj
                (generate-sql {:create-table table :columns [[:__placeholder "int"]]})
                (generate-sql {:alter-table table
                               :table-options table-options
                               :add-columns columns
                               :add-constraints (:constraints command)}))
         (apply conj
                [(format "ALTER TABLE %s ADD %s" (to-sql-name table) pk)]
                (generate-sql {:alter-table table
                               :drop-columns [:__placeholder]})))
        (let [sanitized-columns
              (for [[name & opts] columns]
                (cons name (filter (comp not unsupported-column-spec) opts)))]
          [(if pk
             (apply jdbc/create-table-ddl table (apply conj sanitized-columns [pk] constraints))
             (if constraints
               (apply jdbc/create-table-ddl table (apply conj sanitized-columns constraints))
               (apply jdbc/create-table-ddl table sanitized-columns)))])))))

(defmethod generate-sql
  :drop-table
  [{table :drop-table}]
  (log/info " - Dropping table" (log/highlight (name table)))
  [(jdbc/drop-table-ddl table)])

(defmethod generate-sql
  :alter-table
  [{table :alter-table add-columns :add-columns drop-columns :drop-columns modify-columns :modify-columns
    add-constraints :add-constraints  drop-constraints :drop-constraints
    table-options :table-options character-set :character-set}]
  (log/info " - Altering table" (log/highlight (name table)))
  (let [additions
        (when (not-empty add-columns)
          [(format "alter table %s add column (%s)"
                   (to-sql-name table)
                   (s/join ", "
                           (filter identity
                                   (for [[column & specs] add-columns]
                                     (if (and *db*
                                              (column-exists? *db* (to-sql-name table) (to-sql-name column))
                                              (not (some (fn [prior]
                                                           (and (= table (:alter-table prior))
                                                                (some (fn [col] (= col column)) (:drop-columns prior))))
                                                         @*plan*)))
                                       (do (log/info (format "   * Skipping ADD COLUMN %s.%s because it already exists."
                                                             (to-sql-name table) (to-sql-name column)))
                                           nil)
                                       (do (log/info "    * Adding column" (log/highlight (name column)))
                                           (format "%s %s"
                                                   (to-sql-name column)
                                                   (s/join " " (remove-unsupported-column-specs specs)))))))))])
        removals (when (not-empty drop-columns)
                   [(format "alter table %s drop column %s"
                         (to-sql-name table)
                         (s/join ", "
                                 (filter identity
                                         (for [column drop-columns]
                                           ;; Note: No test for prior alter table/add column or create table w/ column
                                           ;; because frankly it would not make sense in a single migration to create
                                           ;; a table column and then immediately drop it.
                                           (if (or (not *db*)
                                                   (column-exists? *db* (to-sql-name table) (to-sql-name column)))
                                             (do (log/info "    * Dropping column" (log/highlight (name column)))
                                                 (format "%s" (to-sql-name column)))
                                             (do (log/info "   * Skipping DROP COLUMN %s.%s because that column does not exist."
                                                           (to-sql-name table) (to-sql-name column))
                                                 nil))))))])
        modifications (filter identity (for [[column & specs] modify-columns]
                                         (do (log/info "    * Modifying column" (log/highlight (name column)))
                                             (format "ALTER TABLE %s ALTER COLUMN %s %s"
                                                     (to-sql-name table)
                                                     (to-sql-name column)
                                                     (s/join " " (remove-unsupported-column-specs specs))))))
        new-constraints (filter
                         identity
                         (mapcat
                          (fn [[constraint & specs]]
                            (let [spec-sql (s/join " " specs)
                                  [_ fkcolumn pktable pkcolumn] (re-matches #"(?i)\(([\w_]+)\)\s+REFERENCES\s+([\w_]+)\(([\w_]+)\).*" spec-sql)
                                  _ (if (or (nil? fkcolumn) (nil? pktable) (nil? pkcolumn))
                                      (log/error "Failed to parse constraint for" constraint ":" spec-sql))
                                  existing-constraints (get-matching-foreign-keys *db* (to-sql-name table) (to-sql-name fkcolumn)
                                                                                  (to-sql-name pktable) (to-sql-name pkcolumn))
                                  undropped-existing-constraints (filter (fn [existing]
                                                                           (not (some (fn [prior]
                                                                                        (some (fn [to-drop]
                                                                                                (= (s/upper-case (to-sql-name to-drop))
                                                                                                   (s/upper-case (to-sql-name existing))))
                                                                                              (:drop-constraints prior)))
                                                                                      @*plan*)))
                                                                         existing-constraints)]
                              (if (or (not *db*)
                                      (not (foreign-key-exists? *db* (to-sql-name constraint)))
                                      (some (fn [prior]
                                              ;; Did we previously drop the constraint?
                                              (and (= table (:alter-table prior))
                                                   (some (fn [cons] (= cons constraint))
                                                         (:drop-constraints prior))))
                                            @*plan*))
                                (do
                                  (log/info "    * Adding constraint " (log/highlight (if constraint constraint "unnamed")))
                                  (concat (into []
                                                (for [undropped-constraint undropped-existing-constraints]
                                                  (do (log/info "    * Dropping foreign key" undropped-constraint "because it is being replaced")
                                                      (format "ALTER TABLE %s DROP FOREIGN KEY %s"
                                                              (to-sql-name table)
                                                              (s/upper-case (to-sql-name undropped-constraint))))))
                                          [(format "ALTER TABLE %s ADD CONSTRAINT %s FOREIGN KEY %s" 
                                                   (to-sql-name table)
                                                   (to-sql-name constraint)
                                                   spec-sql)]))
                                (do (log/info "    * Skipping adding constraint " (log/highlight (if constraint constraint "unnamed"))
                                              " because it already exists")
                                    nil))))
                          add-constraints))
        old-constraints (filter identity
                                (for [constraint drop-constraints]
                                  ;; As with drop-column, we are not checking here for add constraint followed by drop constraint
                                  ;; in the same migration.  Wouldn't make sense.
                                  (if (or (not *db*)
                                          (foreign-key-exists? *db* (to-sql-name constraint)))
                                    (do
                                      (log/info "    * Removing constraint " (log/highlight constraint))
                                      (format "ALTER TABLE %s DROP FOREIGN KEY %s",
                                              (to-sql-name table)
                                              (to-sql-name constraint)))
                                    (log/info "   * Skipping removing constraint " (log/highlight constraint) " because it does not exist"))))
        options
        (when (not-empty table-options)
          (log/info "   * Skipping options " table-options " because H2 does not support them")
          [])
        charset
        (when character-set
          (log/info "   * Skipping convert to character set because H2 does not support it.")
          [])]
    (doall (concat options charset old-constraints removals additions modifications new-constraints))))

(defmethod generate-sql
  :create-index
  [{index   :create-index
    table   :on
    columns :columns
    unique  :unique}]
  (if (and *db*
           (index-exists? *db* (to-sql-name table) (to-sql-name index))
           (not (some (fn [prior]
                        (and (= index (:drop-index prior))
                             (= table (:on prior))))
                      @*plan*)))
    (do (log/info (format "   * Skipping CREATE INDEX on table %s name %s because it already exists."
                          (to-sql-name table) (to-sql-name index)))
        nil)
    (let [columns (map (comp remove-column-length to-sql-name) columns)
          indexable-columns (filter (fn [column]
                                      (let [type (or (:type_name (get-column-definition *db* (to-sql-name table) column))
                                                     (some (fn [prior]
                                                             (cond (= table (:create-table prior))
                                                                   (some (fn [col]
                                                                           (and (= (to-sql-name (first col)) column)
                                                                                (second col)))
                                                                         (:columns prior))
                                                                   (= table (:alter-table prior))
                                                                   (some (fn [col]
                                                                           (and (= (to-sql-name (first col)) column)
                                                                                (second col)))
                                                                         (:add-columns prior))))
                                                           @*plan*))]
                                        (not (not-indexable-type? type))))
                                    columns)]
      (if (empty? indexable-columns)
        (do (log/info (format "   * Skipping creating index %s because none of the specified columns are indexable." index))
            [])
        (do (log/info " - Creating" (if unique "unique" "") "index" (log/highlight (name index)) "on" (log/highlight (name table)))
            [(format "CREATE %s INDEX %s ON %s (%s)"
                     (if unique "UNIQUE" "")
                     (to-sql-name index)
                     (to-sql-name table)
                     (s/join ", " indexable-columns))])))))

(defmethod generate-sql
  :drop-index
  [{index :drop-index table :on}]
  ;; As with drop-column, we are not checking here for create index followed by drop index
  ;; in the same migration.  Wouldn't make sense.
  (if (or (not *db*) (index-exists? *db* (to-sql-name table) (to-sql-name index)))
    (do (log/info " - Dropping index" (log/highlight (name index)))
        [(format "DROP INDEX %s ON %s" (to-sql-name index) (to-sql-name table))])
    (do (log/info (format "   * Skipping DROP INDEX on table %s index %s because that index does not exist."
                          table index))
        nil)))

(defmethod generate-sql
  :insert-into
  [{table :insert-into values :values query :query}]
  (log/info " - Inserting into" (log/highlight (name table)))
  (cond (not-empty values)
        [(format "INSERT INTO %s VALUES %s" (to-sql-name table)
                 (s/join "," (map (fn [vals] (format "(%s)" (s/join ","
                                                                    (map #(if (string? %)
                                                                            (str "'" % "'")
                                                                            (pr-str %))
                                                                         vals))))
                                  values)))]
        (not-empty query)
        [(format "INSERT INTO %s %s" (to-sql-name table) query)]
        :else (throw (Exception. ":insert-into must contain a non-empty :values or :query key"))))

(defmethod generate-sql
  :update
  [{update-query :update h2-version :h2}]
  (log/info " - Updating the database" (log/highlight update-query))
  (let [command (or (not-empty h2-version) (not-empty update-query))]
    (cond (not-empty command)
          [command]
          :else (throw (Exception. ":update must contain a non-empty update statement or :h2 version of the same statement")))))

(defmethod generate-sql
  :default
  [command]
  (log/error "Don't know how to process command:" (log/highlight (pr-str command)))
  (System/exit 1))

(defn- database-exists?
  [db]
  (jdbc/query
    (db-spec db)
    ["SELECT 1 FROM INFORMATION_SCHEMA.SCHEMATA WHERE SCHEMA_NAME = ?" (:schema db "PUBLIC")]
    :result-set-fn first))

(defn get-matching-foreign-keys
  [db fktable fkcolumn pktable pkcolumn]
  (map :fk_name
       (jdbc/query
        (db-spec db)
        [(str "select fk_name from information_schema.cross_references where fktable_schema=SCHEMA() AND fktable_name=? AND fkcolumn_name=? AND "
              "pktable_schema=SCHEMA() AND pktable_name=? AND pkcolumn_name=?")
         (s/upper-case fktable)
         (s/upper-case fkcolumn)
         (s/upper-case pktable)
         (s/upper-case pkcolumn)])))

(defn foreign-key-exists?
  [db fk]
  (if db
    (jdbc/query
     (db-spec db)
     ["SELECT 1 from information_schema.constraints WHERE CONSTRAINT_SCHEMA=SCHEMA() AND CONSTRAINT_NAME=? AND CONSTRAINT_TYPE='REFERENTIAL'"
      (s/upper-case fk)]
     :result-set-fn first)
    false))

(defn- index-exists?
  [db table index]
  (if db
    (do 
      ;;(log/info (format "   * Testing whether table %s has index %s" table index))
      (jdbc/query
       (db-spec db)
       ["SELECT 1 from information_schema.indexes WHERE TABLE_SCHEMA=SCHEMA() AND TABLE_NAME=? AND INDEX_NAME=?"
        (s/upper-case table)
        (s/upper-case index)]
       :result-set-fn first))
    false))

(defn- table-exists?
  [db table]
  (jdbc/query
    (db-spec db)
    ["SELECT 1 FROM information_schema.tables WHERE table_schema=SCHEMA() and table_name = ?" (s/upper-case table)]
    :result-set-fn first))

(defn not-indexable-type?
  [datatype]
  (#{"CLOB" "TINYTEXT" "TEXT" "MEDIUMTEXT" "LONGTEXT" "NTEXT" "NCLOB" "BLOB" "TINYBLOB" "MEDIUMBLOB" "LONGBLOB" "IMAGE" "OID"}
   (s/upper-case (name datatype))))

(defn- get-column-definition
  [db table column]
  (jdbc/query
   (db-spec db)
   ["SELECT * from information_schema.columns where table_schema=SCHEMA() and table_name=? and column_name=?"
    (s/upper-case table)
    (s/upper-case column)
    ]
   :result-set-fn first
   ))

(defn- column-exists?
  [db table column]
  (jdbc/query
   (db-spec db)
   ["SELECT 1 from information_schema.columns where table_schema=SCHEMA() and table_name=? and column_name=?"
    (s/upper-case table)
    (s/upper-case column)]
   :result-set-fn first))

(defn- create-database
  [db]
  (jdbc/db-do-commands
    (db-spec db) false
    (format "CREATE SCHEMA %s" (:schema db "PUBLIC"))))

(defn- create-version-table
  [db version-table]
  (apply jdbc/db-do-commands
         (db-spec db)
         (generate-sql
           {:create-table version-table
            :columns [[:version "VARCHAR(14)" "NOT NULL"]
                      [:created "timestamp"      "NOT NULL" "default CURRENT_TIMESTAMP"]]})))

(defn- psql-error-message
  [e]
  (s/replace (.getMessage e) #"^(FATAL|ERROR): " ""))

(defn- batch-update-error-message
  [e]
  (s/replace (.getMessage (.getNextException e)) #"^(FATAL|ERROR): " ""))

(defn- init-db!
  [{:keys [db version-table]}]
  (when-not (database-exists? db)
    (create-database db)
    (log/info "Created database:" (:schema db "PUBLIC")))
  (when-not (table-exists? db version-table)
    (create-version-table db version-table)
    (log/info "Created table:   " version-table)))

(defn- get-version
  [{:keys [db version-table]}]
  (let [dbspec (db-spec db)
        connection (jdbc/get-connection dbspec)]
    (try
      (jdbc/query
       dbspec
       [(format "SELECT version FROM %s
                  ORDER BY version DESC
                  LIMIT 1" version-table)]
       :row-fn :version
       :result-set-fn first))))

(defn- update-schema-version
  [version-table version]
  (format "INSERT INTO %s (version,created) VALUES (%s,CURRENT_TIMESTAMP)"
          version-table version))

(defn- run-migration!
  [{:keys [db version-table]} version commands]
  (when-not (vector? commands)
    (log/error "Values for `up` and `down` must be vectors of commands"))
  (try
    (binding [*db* db
              *plan* (atom [])]
      #_(doseq [column (jdbc/query (db-spec db)
                                 ["select * from information_schema.columns where table_schema=SCHEMA() and table_name=? and column_name=?"
                                  "TAXNODES" "PATH"])]
        (println (pr-str column)))
      (let [sql-commands (into [] (mapcat
                                   (fn [command]
                                     (let [sql (generate-sql command)]
                                       (swap! *plan* concat [command])
                                       sql))
                                   commands))]
        (doseq [cmd sql-commands]
          (log/info "Executing: " cmd)
          (jdbc/db-do-commands (db-spec db) cmd))
        (log/info "Updating version to: " version)
        (jdbc/db-do-commands (db-spec db) (update-schema-version version-table version))))))

(defn- validate-commands
  [commands]
  (cond (and (vector? commands)
             (every? map? commands)) commands
        (map? commands) [vec commands]
        nil nil
        :else (do
                (log/error "The values for `up` and `down` must be either a map or a vector of maps.")
                (System/exit 1))))

(defrecord H2Migrator
  [config]
  Migrator
  (init[this]
    (init-db! config))
  (version [this]
    (or (get-version config) "0"))
  (migrate [this version commands]
    (run-migration! config version (validate-commands commands))))
