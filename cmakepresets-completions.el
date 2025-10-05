;;; cmakepresets-completions.el --- Retrieve targets -*- lexical-binding: t -*-

;;; Commentary:

;; Defines completions for `${<some-macro-name>}` and `$env{<some-env-var>}`.
;;
;; TIP: Kitware provides a JSON schema for CMake presets in their github repo.
;; You can add `"$schema":
;; "https://raw.githubusercontent.com/Kitware/CMake/refs/heads/master/Help/manual/presets/schema.json"'
;; to your cmake presets file and turn-on eglot (with a language server, such as
;; `vscode-json-languageserver'), this will provide completions and other
;; functionality.

;;; Code:

(require 'thingatpt)

(defconst cp-macro-expansions
  '("sourceDir"
    "sourceParentDir"
    "sourceDirName"
    "presetName"
    "generator"
    "hostSystemName"
    "fileDir"
    "dollar"
    "pathListSep")
  "List of available macro expansions for `${<macro-name>}` completions.")


(defun cp-mode-completions-at-point-for-macros ()
  "Provide completion for `${<macro-name>}`.

The possible completions are in `cmakepresets-macro-expansions'."
  ;; Completions for ${<some-mscro-name>} or ${} (empty)
  (when (thing-at-point-looking-at "\\${\\(.*?\\)}")
    (let* ((start (match-beginning 1))
           (end (match-end 1)))
      (list (or start (point)) ;; Start at the cursor if `${}` is empty
            (or end (point))   ;; End also at the cursor
            cp-macro-expansions))))


(defun cp-mode-completions-at-point-for-env-variables ()
  "Provide completion for `$env{<some-environment-variable>}'.

Note that the environment variables are taken from
`process-environment`, which is how Emacs sees the environment
variables. There might be differences with the environment variables
seen by CMake."
  (when (thing-at-point-looking-at "\\$env{\\(.*?\\)}")
    (let* ((start (match-beginning 1))
           (end (match-end 1))
           (env-vars (mapcar (lambda (entry) (car (split-string entry "="))) process-environment)))
      (list (or start (point)) ;; Start at the cursor if `$env{}` is empty
            (or end (point))   ;; End also at the cursor
            env-vars))))


(defun cp-mode-setup-completions ()
  "Setup `${<some-words>}` completions for my major mode."
  (add-hook 'completion-at-point-functions #'cp-mode-completions-at-point-for-macros nil t)
  (add-hook 'completion-at-point-functions #'cp-mode-completions-at-point-for-env-variables nil t)
  )


(provide 'cmakepresets-completions)
;;; cmakepresets-completions.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("cp-" . "cmakepresets-"))
;; End:
