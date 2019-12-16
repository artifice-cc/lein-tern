(ns tern.mysql-test
  (:require [tern.mysql   :refer :all]
            [expectations :refer :all]))

(expect ["CREATE TABLE foo (a INT)"]
        (generate-sql {:create-table :foo :columns [[:a "INT"]]}))

(expect ["CREATE TABLE foo (a INT, PRIMARY KEY (a))"]
        (generate-sql {:create-table :foo :columns [[:a "INT"]] :primary-key [:a]}))

(expect ["CREATE TABLE foo (a INT, CONSTRAINT fk_a FOREIGN KEY (a) REFERENCES foo(a))"]
        (generate-sql {:create-table :foo :columns [[:a "INT"]] :constraints [[:fk_a "(a) REFERENCES foo(a)"]]}))

(expect ["CREATE TABLE foo (a INT, PRIMARY KEY (a), CONSTRAINT fk_a FOREIGN KEY (a) REFERENCES foo(a))"]
        (generate-sql {:create-table :foo :columns [[:a "INT"]] :primary-key [:a] :constraints [[:fk_a "(a) REFERENCES foo(a)"]]}))

(expect ["INSERT INTO foo VALUES (1,2,\"foo\"),(3,4,\"bar\")"]
        (generate-sql {:insert-into :foo :values [[1 2 "foo"] [3 4 "bar"]]}))

(expect ["ALTER TABLE foo ADD CONSTRAINT fk_foo_bar FOREIGN KEY (bar_id) REFERENCES bar(id)"]
        (generate-sql {:alter-table :foo :add-constraints [[:fk_foo_bar "(bar_id) REFERENCES bar(id)"]]}))

(expect ["ALTER TABLE foo ROW_FORMAT=Compressed, ADD CONSTRAINT fk_foo_bar FOREIGN KEY (bar_id) REFERENCES bar(id)"]
        (generate-sql {:alter-table :foo
                       :table-options [{:name "ROW_FORMAT" :value "Compressed"}]
                       :add-constraints [[:fk_foo_bar "(bar_id) REFERENCES bar(id)"]]}))

(expect ["CREATE TABLE foo (__placeholder int)"
         "ALTER TABLE foo ROW_FORMAT=Compressed, ADD COLUMN a INT, ADD COLUMN b INT"
         "ALTER TABLE foo ADD PRIMARY KEY (a)"
         "ALTER TABLE foo DROP COLUMN __placeholder"]
        (generate-sql {:create-table :foo
                       :primary-key [:a]
                       :table-options [{:name "ROW_FORMAT" :value "Compressed"}]
                       :columns [[:a "INT"] [:b "INT"]]}))

