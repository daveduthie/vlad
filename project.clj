(defproject ddrbt/vlad "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url  "http://www.eclipse.org/legal/epl-v10.html"}
  :profiles
  {:dev {:dependencies [[org.clojure/clojure "1.9.0"]
                        [funcool/struct "1.3.0"]]

         :jvm-opts ["-Dclojure.server.repl={:port 5555 :accept clojure.core.server/repl}"]}})
