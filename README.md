# vlad

## Usage

```
(deftest optional-test
  (let [data               {:a "banana"
                            :b {:d :1}
                            :e [:strawberry :fruit/loves "banana"]}
        +schema+           {:a [string-validator
                                (min-length-validator 4)]
                            :b (opt {:c [int-validator]
                                     :d (req [int-validator])})
                            :e (idx (repeat 3 [(type-validator (type :foo))]))}
        [conformed errors] (validate +schema+ data)]
    (is (= {:b {:d "Must be an integer"}
            :e {2 (str "Must be a " (type :foo))}}
           errors))))
```

## License

Copyright © 2018 David Duthie

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
