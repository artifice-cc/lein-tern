(ns tern.mysql
  (:require [tern.db           :refer :all]
            [tern.log          :as log]
            [clojure.java.jdbc :as jdbc]
            [clojure.string    :as s])
  (:import [java.util Date]
           [java.sql PreparedStatement Timestamp]))

(declare foreign-key-exists?)

(def ^:dynamic *db* nil)

(def ^{:doc "Set of supported commands. Used in `generate-sql` dispatch."
       :private true}
  supported-commands
  #{:create-table :drop-table :alter-table :create-index :drop-index :insert-into})

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

(defmulti generate-sql
  (fn [c] (some supported-commands (keys c))))

(defmethod generate-sql
  :create-table
  [{table :create-table columns :columns :as command}]
  (log/info " - Creating table" (log/highlight (name table)))
  (let [constraints (generate-constraints command)
        pk (generate-pk command)]
     [(if pk
          (apply jdbc/create-table-ddl table (apply conj columns [pk] constraints))
          (if constraints
              (apply jdbc/create-table-ddl table (apply conj columns constraints))
              (apply jdbc/create-table-ddl table columns)))]))

(defmethod generate-sql
  :drop-table
  [{table :drop-table}]
  (log/info " - Dropping table" (log/highlight (name table)))
  [(jdbc/drop-table-ddl table)])

(defmethod generate-sql
  :alter-table
  [{table :alter-table add-columns :add-columns drop-columns :drop-columns modify-columns :modify-columns
          add-constraints :add-constraints  drop-constraints :drop-constraints}]
  (log/info " - Altering table" (log/highlight (name table)))
  (let [additions
        (for [[column & specs] add-columns]
          (do (log/info "    * Adding column" (log/highlight (name column)))
            (format "ALTER TABLE %s ADD COLUMN %s %s"
                    (to-sql-name table)
                    (to-sql-name column)
                    (s/join " " specs))))
        removals
        (for [column drop-columns]
          (do (log/info "    * Dropping column" (log/highlight (name column)))
            (format "ALTER TABLE %s DROP COLUMN %s"
                    (to-sql-name table)
                    (to-sql-name column))))
        modifications
        (for [[column & specs] modify-columns]
          (do (log/info "    * Modifying column" (log/highlight (name column)))
              (format "ALTER TABLE %s MODIFY COLUMN %s %s"
                      (to-sql-name table)
                      (to-sql-name column)
                      (s/join " " specs))))
        new-constraints
        (for [[constraint & specs] add-constraints]
          (if (not (foreign-key-exists? *db* (to-sql-name constraint)))
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
         (if (foreign-key-exists? *db* (to-sql-name constraint))
           (do
             (log/info "    * Removing constraint " (log/highlight constraint))
             (format "ALTER TABLE %s DROP FOREIGN KEY %s",
                     (to-sql-name table)
                     (to-sql-name constraint)))
           (log/info "   * Skipping removing constraint " (log/highlight constraint) " because it does not exist")))]
    (doall (filter identity (concat old-constraints removals additions modifications new-constraints)))))

(defmethod generate-sql
  :create-index
  [{index   :create-index
    table   :on
    columns :columns
    unique  :unique}]
  (log/info " - Creating" (if unique "unique" "") "index" (log/highlight (name index)) "on" (log/highlight (name table)))
  [(format "CREATE %s INDEX %s ON %s (%s)"
           (if unique "UNIQUE" "")
           (to-sql-name index)
           (to-sql-name table)
           (s/join ", " (map to-sql-name columns)))])

(defmethod generate-sql
  :drop-index
  [{index :drop-index table :on}]
  (log/info " - Dropping index" (log/highlight (name index)))
  [(format "DROP INDEX %s ON %s" (to-sql-name index) (to-sql-name table))])

(defmethod generate-sql
  :insert-into
  [{table :insert-into values :values}]
  (log/info " - Inserting into" (log/highlight (name table)))
  [(format "INSERT INTO %s VALUES %s" (to-sql-name table)
           (s/join "," (map (fn [vals] (format "(%s)" (s/join "," (map pr-str vals)))) values)))])

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


(defn- table-exists?
  [db table]
  (jdbc/query
    (db-spec db)
    ["SELECT 1 FROM information_schema.tables WHERE table_name = ?" table]
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
    (binding [*db* db]
      (let [sql-commands (into [] (mapcat generate-sql commands))]
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
