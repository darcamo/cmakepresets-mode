;;; cmakepresets-mode.el --- Major mode for CMake presets file  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Moreira

;; Author: Moreira <AUTH\moreirad@lx-3117035b368e>
;; Version: 0.1
;; Keywords: c c++ cmake languages tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Defines a major mode for CMake presets files, which includes imenu
;; support.

;;; Code:

(require 'treesit)


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


;;;###autoload
(define-derived-mode cp-mode json-ts-mode "CMakePresets"
  "Major mode for editing CMakePresets.json files.

This mode inherits from `json-mode` and provides additional functionality
specific to CMake presets, such as imenu support."
  (cp-setup-imenu))


;;;###autoload
(add-to-list 'auto-mode-alist '("CMakePresets\\.json\\'" . cp-mode))
(add-to-list 'auto-mode-alist '("CMakeUserPresets\\.json\\'" . cp-mode))


(provide 'cmakepresets-mode)
;;; cmakepresets-mode.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("cp-" . "cmakepresets-"))
;; End:
