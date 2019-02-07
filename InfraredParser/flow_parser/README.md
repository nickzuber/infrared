# flow_parser
Flow parser with Fastpack-specific modifications

# Upgrading the Flow version

Change version in the `Makefile` and `package.json` and run:
```bash
$ make clean && make flow
```

Copy the parser code to `lib/`:
```bash
$ cp -R flow/src/parser/* lib
```

Manually modify `lib/parser_env.mli`:
```
36a37
+   types_in_comments: bool;

```

And `lib/parser_env.ml`:
```
161a162
+   types_in_comments: bool;
170a172
+   types_in_comments = true;
229c231
-   let enable_types_in_comments = parse_options.types in
---
+   let enable_types_in_comments = parse_options.types && parse_options.types_in_comments in
```

Make sure it compiles and test passes:
```bash
$ make test
...
types_in_comments=false: OK
types_in_comments=true: OK
```

To publish the package, run:
```
$ npm publish
```
