# CSS Parser

## 実行

```
$ clj -m core                                                                                                                                                                                             8.3s
#css_parser.core.Ruleset{:selector .container h1,
                         :rules (#css_parser.core.Rule{:key color, :value rgba(255, 0, 0, 0.9)}
                                 #css_parser.core.Rule{:key font-size, :value 24px}
                                 #css_parser.core.Rule{:key font-family, :value Monaco})}
```

## 参考

* [Parsing CSS file with monadic parser in Clojure](https://gist.github.com/kachayev/b5887f66e2985a21a466)
