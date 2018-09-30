(ns tern.mysql
  (:require [tern.db           :refer :all]
            [tern.log          :as log]
            [clojure.java.jdbc :as jdbc]
            [clojure.string    :as s])
  (:import [java.util Date]
           [java.sql PreparedStatement Timestamp]))

(declare foreign-key-exists? index-exists? column-exists? table-exists?)

(def ^:dynamic *db* nil)
(def ^:dynamic *plan* nil)

(def ^{:doc "Set of supported commands. Used in `generate-sql` dispatch."
       :private true}
  supported-commands
  #{:create-table :drop-table :alter-table :create-index :drop-index :insert-into :update})

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
        [(if pk
           (apply jdbc/create-table-ddl table (apply conj columns [pk] constraints))
           (if constraints
             (apply jdbc/create-table-ddl table (apply conj columns constraints))
             (apply jdbc/create-table-ddl table columns)))]))))

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
                (format "ALTER TABLE %s ADD COLUMN %s %s"
                        (to-sql-name table)
                        (to-sql-name column)
                        (s/join " " specs)))))
        removals
        (for [column drop-columns]
          ;; Note: No test for prior alter table/add column or create table w/ column
          ;; because frankly it would not make sense in a single migration to create
          ;; a table column and then immediately drop it.
          (if (or (not *db*)
                  (column-exists? *db* (to-sql-name table) (to-sql-name column)))
            (do (log/info "    * Dropping column" (log/highlight (name column)))
                (format "ALTER TABLE %s DROP COLUMN %s"
                        (to-sql-name table)
                        (to-sql-name column)))
            (do (log/info "   * Skipping DROP COLUMN %s.%s because that column does not exist."
                          (to-sql-name table) (to-sql-name column))
                nil)))
        modifications
        (for [[column & specs] modify-columns]
          (do (log/info "    * Modifying column" (log/highlight (name column)))
              (format "ALTER TABLE %s MODIFY COLUMN %s %s"
                      (to-sql-name table)
                      (to-sql-name column)
                      (s/join " " specs))))
        new-constraints
        (for [[constraint & specs] add-constraints]
          (if (or (not *db*)
                  (not (foreign-key-exists? *db* (to-sql-name constraint)))
                  (some (fn [prior]
                          ;; Did we previously drop the constraint?
                          (and (= table (:alter-table prior))
                               (some (fn [[cons & specs]] (= cons constraint))
                                     (:drop-constraints prior))))
                        @*plan*))
            (do
              (log/info "    * Adding constraint " (log/highlight (if constraint constraint "unnamed")))
              (format "ALTER TABLE %s ADD CONSTRAINT %s FOREIGN KEY %s" 
                      (to-sql-name table)
                      (to-sql-name constraint)
                      (s/join  " " specs)))
            (log/info "    * Skipping adding constraint " (log/highlight (if constraint constraint "unnamed"))
                      " because it already exists")))
       old-constraints
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
           (log/info "   * Skipping removing constraint " (log/highlight constraint) " because it does not exist")))
        options
        (when (not-empty table-options)
          [(str (format "ALTER TABLE %s " (to-sql-name table))
                (generate-options table-options))])
        charset
        (when character-set
          [(str
            (format "ALTER TABLE %s CONVERT TO CHARACTER SET %s" (to-sql-name table) (:charset-name character-set))
            (if (:collation character-set)
              (format " COLLATE %s" (:collation character-set))
              ""))])]
    (doall (filter identity (concat options charset old-constraints removals additions modifications new-constraints)))))

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
    (do (log/info " - Creating" (if unique "unique" "") "index" (log/highlight (name index)) "on" (log/highlight (name table)))
        [(format "CREATE %s INDEX %s ON %s (%s)"
                 (if unique "UNIQUE" "")
                 (to-sql-name index)
                 (to-sql-name table)
                 (s/join ", " (map to-sql-name columns)))])))

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
                 (s/join "," (map (fn [vals] (format "(%s)" (s/join "," (map pr-str vals)))) values)))]
        (not-empty query)
        [(format "INSERT INTO %s %s" (to-sql-name table) query)]
        :else (throw (Exception. ":insert-into must contain a non-empty :values or :query key"))))

(defmethod generate-sql
  :update
  [{update-query :update}]
  (log/info " - Updating the database" (log/highlight update-query))
  (cond (not-empty update-query)
        [update-query]
        :else (throw (Exception. ":update must contain a non-empty update query"))))

(defmethod generate-sql
  :default
  [command]
  (log/error "Don't know how to process command:" (log/highlight (pr-str command)))
  (System/exit 1))

(defn- database-exists?
  [db]
  (jdbc/query
    (db-spec db "mysql")
    ["SELECT 1 FROM INFORMATION_SCHEMA.SCHEMATA WHERE SCHEMA_NAME = ?" (:database db)]
    :result-set-fn first))

(defn- foreign-key-exists?
  [db fk]
  (if db
    (jdbc/query
     (db-spec db)
     ["SELECT 1 from information_schema.table_constraints WHERE CONSTRAINT_SCHEMA=DATABASE() AND CONSTRAINT_NAME=? AND CONSTRAINT_TYPE='FOREIGN KEY'" fk]
     :result-set-fn first)
    false))

(defn- index-exists?
  [db table index]
  (if db
    (do 
      ;;(log/info (format "   * Testing whether table %s has index %s" table index))
      (jdbc/query
       (db-spec db)
       ["SELECT 1 from information_schema.statistics WHERE TABLE_SCHEMA=DATABASE() AND TABLE_NAME=? AND INDEX_NAME=?" table index]
       :result-set-fn first))
    false))

(defn- table-exists?
  [db table]
  (jdbc/query
    (db-spec db)
    ["SELECT 1 FROM information_schema.tables WHERE table_schema=database() and table_name = ?" table]
    :result-set-fn first))

(defn- column-exists?
  [db table column]
  (jdbc/query
   (db-spec db)
   ["SELECT 1 from information_schema.columns where table_schema=database() and table_name=? and column_name=?" table column]
   :result-set-fn first))

(defn- create-database
  [db]
  (jdbc/db-do-commands
    (db-spec db "mysql") false
    (format "CREATE DATABASE %s" (:database db))))

(defn- create-version-table
  [db version-table]
  (apply jdbc/db-do-commands
         (db-spec db)
         (generate-sql
           {:create-table version-table
            :columns [[:version "VARCHAR(14)" "NOT NULL"]
                      [:created "BIGINT"      "NOT NULL"]]})))

(defn- psql-error-message
  [e]
  (s/replace (.getMessage e) #"^(FATAL|ERROR): " ""))

(defn- batch-update-error-message
  [e]
  (s/replace (.getMessage (.getNextException e)) #"^(FATAL|ERROR): " ""))

(defn- init-db!
  [{:keys [db version-table]}]
  (try
    (when-not (database-exists? db)
      (create-database db)
      (log/info "Created database:" (:database db)))
    (when-not (table-exists? db version-table)
      (create-version-table db version-table)
      (log/info "Created table:   " version-table))))

(defn- get-version
  [{:keys [db version-table]}]
  (try
    (jdbc/query
      (db-spec db)
      [(format "SELECT version FROM %s
                  ORDER BY version DESC
                  LIMIT 1" version-table)]
      :row-fn :version
      :result-set-fn first)))

(defn- update-schema-version
  [version-table version]
  (format "INSERT INTO %s (version) VALUES (%s)"
          version-table version))

(defn- run-migration!
  [{:keys [db version-table]} version commands]
  (when-not (vector? commands)
    (log/error "Values for `up` and `down` must be vectors of commands"))
  (try
    (binding [*db* db
              *plan* (atom [])]
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

(defrecord MysqlMigrator
  [config]
  Migrator
  (init[this]
    (init-db! config))
  (version [this]
    (or (get-version config) "0"))
  (migrate [this version commands]
    (run-migration! config version (validate-commands commands))))
