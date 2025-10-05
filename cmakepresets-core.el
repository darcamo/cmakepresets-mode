;;; cmakepresets-core.el --- Main code for cmakepresets-mode -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'treesit)

;; TODO: Use "--"  in functions and variables that should be internal only.

(defconst cp-type-alist
  '(("configurePresets" . "Configure")
    ("buildPresets" . "Build")
    ("testPresets" . "Test")
    ("packagePresets" . "Package")
    ("workflowPresets" . "Workflow"))
  "Mapping from JSON keys to preset types.")


(defun cp-presets-query ()
  "Return the Treesit query for capturing all presets in the JSON file.

This query matches top-level keys (e.g., \"configurePresets\",
\"buildPresets\") that contain an array of preset objects. Each preset
object should have key-value pairs, including `\"name\"`."
  '((pair key:(string (string_content))
          ;; Since for a given type we have an array of presets, then the value
          ;; field of the presets is an array
          value: (array
                  ;; A preset is an object containing one or more pairs, where
                  ;; each pair in treesitter is a "key-value" pair in the JSON
                  (object
                   (pair
                    (string (string_content)))) @presets))))


(defun cp-get-preset-nodes ()
  "Capture and return Treesit nodes representing presets in the current buffer."
  (let*((root (treesit-buffer-root-node))
        (query (cp-presets-query)))
    (treesit-query-capture root query nil nil t)))


(defun cp-node-is-name-p (node)
  "Return non-nil if NODE represents a key-value pair where `key` is \"name\"."
  (let ((key (treesit-node-child-by-field-name node "key")))
    (string-equal (treesit-node-text key t) "\"name\"")))


(defun cp-get-preset-node-name (preset-node)
  "Return the name of PRESET-NODE, or \"<unknown name>\" if not found.

The PRESET-NODE is a treesitter node representing a preset in the json
file. This treesitter node has children which are pairs (the key-value
pairs in the json file). To get the name of the preset, we need to find
which child node of PRESET-NODE is the one whose `key' field is
\"name\", and then we return the `value' field of that chield."
  (if-let* ((name-pair (seq-find #'cp-node-is-name-p
                                 (treesit-node-children preset-node t))))
      (treesit-node-text (treesit-node-child-by-field-name name-pair "value") t)
    "<unknown name>"))


(defun cp-get-preset-node-start-pos (preset-node)
  "Get the start position of the PRESET-NODE."
  (treesit-node-start preset-node))


(defun cp-get-preset-node-type (preset-node)
  "Return the type of PRESET-NODE.

The type is one of \"Configure\", \"Build\", \"Test\", \"Package\", or
\"Workflow\". The type is determined by the parent of the preset node."
  (let* ((parent (treesit-node-parent (treesit-node-parent preset-node)))
         (parent-key (treesit-node-child-by-field-name parent "key"))
         (parent-type (treesit-node-text (treesit-node-child parent-key 0 t) t)))
    (alist-get parent-type cp-type-alist nil nil 'equal)))


(defun cp-imenu-create-index-with-groups ()
  "Create imenu index for CMake presets grouped by preset type.

Each CMake preset is included under its type (e.g., \"Configure\").
Entries have the format \"Type/Name\" pointing to the start position of
the preset."
  (let ((preset-nodes (cp-get-preset-nodes))
        (index-alist '()))
    (dolist (preset-node (nreverse preset-nodes))
      (let* ((preset-name (cp-get-preset-node-name preset-node))
             (preset-start (cp-get-preset-node-start-pos preset-node))
             (preset-type (cp-get-preset-node-type preset-node))
             (full-name (format "%s/%s" preset-type preset-name)))
        (push (cons full-name preset-start)
              (alist-get preset-type index-alist))))
    index-alist))


(defun cp-setup-imenu ()
  "Set up imenu for CMake presets files."
  (setq-local imenu-create-index-function
              #'cp-imenu-create-index-with-groups))


(provide 'cmakepresets-core)
;;; cmakepresets-core.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("cp-" . "cmakepresets-"))
;; End:
