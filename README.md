### swtich-dir

Switch between 2 related files. The files may be located in different
directories and have different extensions. They may be located in sub
directories, as long as the trees are the same. Can be used for unit test, or
compiled files. Usage: create a `.dir-locals.el` in your project root, and write
a config similar to:

```
((js2-mode
  (switch-dir-spec
   . (("node/" "node-tests/"  "js" "spec.js")
      ("public/scripts/" "public-tests/" "js" "spec.js")))))
```

The extensions are optional, and they will be assumed to be identical if omited.

Calling `switch-dir` will switch to the "other" file. With a prefix argument the
file will be created if it doesn't exist.