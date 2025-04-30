# cmakepresets-mode

A major mode for editing CMake presets files.

`cmakepresets-mode` is built on top of `json-mode`, as CMake presets are JSON files. It enhances the editing experience by providing additional conveniences tailored specifically for CMake presets.

Currently, the mode adds support for `imenu` to improve navigation within presets files.


## Configuration

Configuring `cmakepresets-mode` is simple; just install the package and ensure the mode is enabled for CMake preset files. The mode automatically adds entries to `auto-mode-alist` for `"CMakePresets.json"` and `"CMakeUserPresets.json"` files. However, this may not work reliably due to potential conflicts with the `json-mode` package, which can take precedence. To ensure proper setup, you can use the following function in your configuration:

```emacs-lisp
(defun is-cmake-presets-file? ()
  "Check if FILENAME is the name of a cmake presets file."
  (if-let* ((buffer-name (buffer-file-name))
            (filename (file-name-nondirectory buffer-name)))
            ;;               Add other files if you need
      (if (member filename '("CMakePresets.json" "CMakeUserPresets.json")) t nil)))
```

Then, add the following lambda to the `json-mode-hook` to activate `cmakepresets-mode` for matching files:

```emacs-lisp
(lambda () (when (is-cmake-presets-file?) (cmakepresets-mode)))
```
