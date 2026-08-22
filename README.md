# mulog for jolt

The published [mulog](https://github.com/BrunoBonacci/mulog) 0.9.0, with the
three Java classes it bundles (`Flake`, `NanoClock`'s consumer paths,
`ClojureThreadLocal`, plus the `ScheduledThreadPoolExecutor` surface its timer
pool reaches) supplied by portable jolt registrations
(`com.brunobonacci.mulog.jolt-shim`). The Flake port is bit-for-bit: the
homomorphic base64 string form, the hex form, parse, and unsigned ordering all
match the JVM class, so flakes printed here read there and sort the same.

```clojure
{:deps {com.brunobonacci/mulog
        {:git/url "https://github.com/jolt-lang/mulog" :git/sha "..."}}}
```
