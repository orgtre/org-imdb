;;; org-imdb.el --- Interface between Org and IMDb -*- lexical-binding: t; -*-

;; Copyright (C) 2023 orgtre

;; Author: orgtre
;; Package-Requires: ((sqlite3 "0.16") (levenshtein "0"))
;; URL: https://github.com/orgtre/org-imdb

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'org)
(require 'cl-lib)
(require 'sqlite3)
(require 'levenshtein)

(defgroup org-imdb nil
  "Interface between Org Mode and the IMDb public datasets."
  :group 'multimedia)

(defcustom org-imdb-db nil
  "Path to an SQLite database holding the IMDb data.
It should be created by URL `https://github.com/jojje/imdb-sqlite'
or at least follow a similar schema."
  :type 'string)

(defcustom org-imdb-id "imdbid"
  "Property used to identify Org entries part of your database.
Used to match entries by `org-imdb-update-all'."
  :type 'string)

(defcustom org-imdb-id-na "n.a."
  "Value used to indicate `org-imdb-id' is not available.
When `org-imdb-id' is set to this value no attempt is made
to update the entry using the IMDb datasets. Should be
nonempty and contain more than whitespace."
  :type 'string)

(defcustom org-imdb-properties
  '("imdbid" "director" "rating" "votes" "genres"
    "runtime" "year" "type")
  "List of database columns to write as properties to Org entries.
Its elements have to be in `org-imdb-columns'."
  :type 'list)

(defcustom org-imdb-user-properties
  '("prio" "tag" "watched" "score" "link")
  "List of properties whose values are manually set by the user."
  ;; TODO Should these properties be added with nil values?
  :type 'list)

(defcustom org-imdb-properties-order
  '("imdbid" "director" "prio" "tag" "watched" "score"
    "runtime" "type" "rating" "votes" "year" "genres" "link")
  "List containing entry properties in the desired order.
Should contain a subset of `org-imdb-properties' and
`org-imdb-user-properties'. Properties not on this list
will be sorted below."
  :type 'list)

(defcustom org-imdb-sort-entry-on-update t
  "If non-nil sort entry property drawer when updating.
The sort order is determined by `org-imdb-properties-order'."
  :type 'bool)

(defcustom org-imdb-show-entry-on-update t
  "If non-nil show entry and its drawer after updating."
  :type 'bool)

(defcustom org-imdb-property-case-function #'identity
  "Convert property names using this function before inserting.
Reasonable values are #\\='upcase, #\\='capitalize, #\\='downcase,
and #\\='identity (the default)."
  :type 'function)

(defcustom org-imdb-imdbid-as-link t
  "If non-nil insert imdbid as url link."
  :type 'bool)

(defcustom org-imdb-heading-constructor
  #'org-imdb-default-heading-constructor
  "Function used to construct and set entry heading.
It should expect an alist with keys `org-imdb-columns'
as argument. When nil, leave heading as it is."
  :type 'function)

(defconst org-imdb-types
  '("movie" "short" "tvEpisode" "tvMiniSeries" "tvMovie"
    "tvPilot" "tvSeries" "tvShort" "tvSpecial" "video" "videoGame")
  "List of all the available IMDb entry types.")

(defconst org-imdb-columns
  '("imdbid" "type" "ptitle" "otitle" "adult" "year"
    "ended" "runtime" "genres" "rating" "votes" "show"
    "season" "episode" "writer" "producer" "editor" "director"
    "composer" "cinematographer" "actor")
  "List of all the available IMDb database columns.
Note that this includes renaming made in `org-imdb-title-query'.")

(defconst org-imdb-title-query
  "SELECT t.title_id                   AS imdbid,
	  t.type,
	  t.primary_title              AS ptitle,
	  t.original_title             AS otitle,
	  t.is_adult                   AS adult,
	  t.premiered                  AS year,
	  t.ended,
	  t.runtime_minutes            AS runtime,
	  REPLACE(t.genres, ',', ', ') AS genres,
	  r.rating,
	  r.votes,
	  s.original_title             AS show,
	  e.season_number              AS season,
	  e.eposide_number             AS episode
   FROM   titles t
	  LEFT JOIN ratings r
		 ON t.title_id = r.title_id
	  LEFT JOIN episodes e
		 ON t.title_id = e.episode_title_id
	  LEFT JOIN titles s
		 ON e.show_title_id = s.title_id
   WHERE  t.title_id = '%s'")

(defconst org-imdb-crew-query
  "SELECT REPLACE(category, 'actress', 'actor') AS cat,
          GROUP_CONCAT(name, ', ')              AS names
   FROM   crew
          INNER JOIN people
                  ON crew.person_id = people.person_id
   WHERE  title_id = '%s'
   GROUP  BY cat")

(defconst org-imdb-get-id-query-base
  "SELECT title_id,
          premiered,
          type,
          original_title,
          primary_title,
          genres
   FROM   titles")

(defconst org-imdb-get-id-query-title
  "(original_title LIKE '%%%1$s%%' OR primary_title LIKE '%%%1$s%%')")

(defvar org-imdb-minor-mode-map
  (make-sparse-keymap))


;;;###autoload
(define-minor-mode org-imdb-minor-mode
  "Minor mode for org-imdb."
  :keymap org-imdb-minor-mode-map
  (if org-imdb-minor-mode
      (org-imdb-setup)
    (org-imdb-teardown)))


(defun org-imdb-setup ()
  "Setup `org-imdb-minor-mode'."
  (let ((pat (format "^:\\(?:%s\\): *\\(.*\\)"
                     (mapconcat 'identity org-imdb-user-properties "\\|"))))
    (font-lock-add-keywords
     nil
     `((,pat 1 'org-date t)
       ("^:.*?:" 0 'org-special-keyword t)
       ("^:\\(?:PROPERTIES\\|END\\):" 0 'org-drawer t))
     t))
  (font-lock-update))


(defun org-imdb-teardown ()
  "Teardown `org-imdb-minor-mode'."
  (let ((pat (format "^:\\(?:%s\\): *\\(.*\\)"
                     (mapconcat 'identity org-imdb-user-properties "\\|"))))
    (font-lock-remove-keywords
     nil
     `((,pat 1 'org-date t)
       ("^:.*?:" 0 'org-special-keyword t)
       ("^:\\(?:PROPERTIES\\|END\\):" 0 'org-drawer t)))
    (font-lock-update)))


;;;###autoload
(defun org-imdb-update-entry (&optional do-not-show-entry)
  "Update entry at point."
  (interactive)
  (let* ((imdbid (or (org-imdb-org-entry-get-imdbid)
		     (org-imdb-get-id)))
         alldata data)
    (unless (equal imdbid org-imdb-id-na)
      (setq alldata (org-imdb-get-title-data imdbid))
      (setq data (mapcar (lambda (x)
			   (assoc x alldata))
		         org-imdb-properties))
      (mapc (lambda (x)
              (when (cdr x)
                ;; omit empty values
	        (org-entry-put
	         nil
	         (funcall org-imdb-property-case-function (car x))
	         (cdr x))))
	    data)
      (when (and org-imdb-imdbid-as-link
	         (member "imdbid" org-imdb-properties))
        (org-imdb-org-entry-linkify-imdbid))
      (when org-imdb-heading-constructor
        (funcall org-imdb-heading-constructor alldata)))
    (when org-imdb-sort-entry-on-update
      (org-imdb-entry-sort-properties))
    (when (or (not do-not-show-entry) org-imdb-show-entry-on-update)
      (org-imdb-entry-toggle-drawer 'off)
      (when (org-fold-folded-p (save-excursion (org-end-of-subtree)))
        (org-fold-heading nil t)))))


(defun org-imdb-org-entry-get-imdbid ()
  "Get imdbid of entry even when it's stored as link.
Returns nil when no valid imdbid is found."
  (let ((imdbid (org-entry-get nil "imdbid")))
    (when imdbid
      (if (string-prefix-p "[[" imdbid)
	  (org-link-display-format imdbid)
        (if (and (not (string-empty-p imdbid))
                 (or (and org-imdb-id-na
                          (equal org-imdb-id-na imdbid))
                     (string-match-p "^tt[0-9]+$" imdbid)))
            imdbid
          nil)))))


(defun org-imdb-org-entry-linkify-imdbid ()
  (let ((imdbid (org-entry-get nil "imdbid")))
    (unless (string-prefix-p "[[" imdbid)
      (org-entry-put
       nil (funcall org-imdb-property-case-function "imdbid")
       (format "[[https://www.imdb.com/title/%1$s][%1$s]]" imdbid)))))


(defun org-imdb-get-id ()
  "Search for imdbid with `completing-read' interface."
  (let* (choice-list
	 (hash-table (make-hash-table :test 'equal))
	 (heading (or (org-entry-get nil "ITEM")
		      (read-string "Word (year): ")))
	 (qtitle (progn
		   (string-match
		    "\\([^(]*\\)\\(?:(\\([0-9]\\{4\\}\\))\\)?" heading)
		   (string-trim (match-string 1 heading))))
	 (year (match-string 2 heading))
	 (selected-type
          (or (when-let ((type (org-entry-get nil "type")))
                (when (member type org-imdb-types) type))
	      (completing-read
               (format "Type for entry \"%s\": " heading)
               org-imdb-types)))
	 (query (org-imdb-get-id-query qtitle year selected-type))
	 (callback (lambda (_ncols row _colnames)
		     (let* ((id (nth 0 row))
			    (premiered (nth 1 row))
			    ;;(type (nth 2 row))
			    (original_title (nth 3 row))
			    (primary_title (nth 4 row))
			    (genres (nth 5 row))
			    (choice-string
			     (concat (format "%s  %s"
					     premiered
					     original_title)
				     (if (equal original_title primary_title)
					 (format "  [%s]" genres)
				       (format " (%s)  [%s]"
					       primary_title
					       genres)))))
		       (puthash choice-string id hash-table)
		       (push choice-string choice-list))))
	 choice selected-id)
    (org-imdb-sql-exec query callback)
    (setq choice (if (not (cdr choice-list))
		     (car choice-list)
		   (completing-read "Title: " choice-list)))
    (setq selected-id (gethash choice hash-table))
    (unless selected-id
      (user-error "No imdbid found. Try a broader query."))
    selected-id))


(defun org-imdb-get-id-query (qtitle year type)
  "Construct SQLite query used to get imdbid.
QTITLE, YEAR, and TYPE will be inserted at suitable points."
  (when (equal qtitle "")
    (setq qtitle nil))
  (when (equal year "")
    (setq year nil))
  (when (equal type "")
    (setq type nil))
  (setq qtitle (string-replace "'" "''" qtitle))
  (concat org-imdb-get-id-query-base
	  " WHERE "
	  (cond
	   ((and year type qtitle)
	    (concat
	     (format "premiered = '%s'" year)
	     " AND "
	     (format "type = '%s'" type)
	     " AND "
	     (format org-imdb-get-id-query-title qtitle)))
	   ((and year type)
	    (concat
	     (format "premiered = '%s'" year)
	     " AND "
	     (format "type = '%s'" type)))
	   ((and year qtitle)
	    (concat
	     (format "premiered = '%s'" year)
	     " AND "
	     (format org-imdb-get-id-query-title qtitle)))
	   ((and type qtitle)
	    (concat
	     (format "type = '%s'" type)
	     " AND "
	     (format org-imdb-get-id-query-title qtitle)))
	   (year (format "premiered = '%s'" year))
	   (type (format "type = '%s'" type))
	   (qtitle (format org-imdb-get-id-query-title qtitle))
	   (t (user-error "Need either a title, type, or year to search.")))))


(defun org-imdb-sql-exec (query callback)
  "Run SQLite QUERY (a string) on the org-imdb database.
Function CALLBACK is called for each row returned by QUERY. Wrapper
around `sqlite3-exec' which opens and closes `org-imdb-db'."
  ;; TODO Why does it randomly work or not with sqlite-open-readonly?
  (unless org-imdb-db
    (user-error
     "Custom variable org-imdb-db needs to be set to a SQLite database."))
  (let ((db (sqlite3-open org-imdb-db sqlite-open-readwrite)))
    (sqlite3-exec db query callback)
    (sqlite3-close db)))


(defun org-imdb-get-title-data (id)
  "Return an alist containing all data for entry with ID.
ID is a string imdbid/title_id."
  (append
   (let* (out
	  (query
	   (format org-imdb-title-query id))
	  (callback (lambda (_ncols row colnames)
		      (setf out (cl-pairlis colnames row)))))
     (org-imdb-sql-exec query callback)
     out)
   (let* (out
	  (query
	   (format org-imdb-crew-query id))
	  (callback (lambda (_ncols row _colnames)
		      (push (cons (car row) (nth 1 row)) out))))
     (org-imdb-sql-exec query callback)
     out)))


(defun org-imdb-default-heading-constructor (d)
  "Construct heading as `otitle (premiered)'.
If the `levenshtein-distance' between otitle and ptitle
is larger than 3, additionally append ` [ptitle]'."
  (let ((otitle (cdr (assoc "otitle" d)))
	(ptitle (cdr (assoc "ptitle" d)))
	(year (cdr (assoc "year" d))))
    (org-edit-headline
     (concat (format "%s (%s)" otitle year)
	     (when (> (levenshtein-distance otitle ptitle) 3)
	       (format " [%s]" ptitle))))))


;;;###autoload
(defun org-imdb-update-all (scope)
  "Update all Org entries with property `org-imdb-id' in SCOPE.
SCOPE is as in `org-map-entires' which is used to run
`org-imdb-update-entry' on the entries."
  (interactive
   (list (intern
          (completing-read
           "Scope: "
           '("nil" "tree" "region" "region-start-level" "file"
             "file-with-archives" "agenda" "agenda-with-archives")))))
  (let ((count 0))
    (org-map-entries
     (lambda ()
       (setq count (1+ count))
       (org-imdb-update-entry t))
     (concat org-imdb-id "<>\"\"")
     scope)
    (message "Updated %s entries." count)))


;;;###autoload
(defun org-imdb-open-web ()
  "Open the IMDb website of entry at point."
  (interactive)
  (let* ((imdbid (org-imdb-org-entry-get-imdbid))
	 (url (concat "https://www.imdb.com/title/" imdbid)))
    (browse-url url)))


(defun org-imdb-entry-sort-properties ()
  "Sorts entry properties according to `org-imdb-properties-order`.
Properties not in that list are but at the end, in their orginal order."
  (interactive)
  (let ((order org-imdb-properties-order)
        (foldp (org-fold-folded-p (car (org-get-property-block)) 'drawer))
        (inprops (org-imdb-entry-properties-preserve-case))
        outprops)
    (setq inprops (nreverse inprops))
    (setq outprops
          (sort inprops
                (lambda (x y)
                  (let* ((xp (string-match-p "\\+\\'" (car x)))
                         (yp (string-match-p "\\+\\'" (car y)))
                         (xbase (if xp (substring (car x) 0 -1) (car x)))
                         (ybase (if yp (substring (car y) 0 -1) (car y)))
                         (posx (cl-position xbase order :test 'string=))
                         (posy (cl-position ybase order :test 'string=)))
                    (cond ((and posx posy)
                           (< posx posy))
                          ((and posx (not posy))
                           t)
                          ((and (not posx) posy)
                           nil)
                          (t
                           nil))))))
    (mapc (lambda (x) (org-entry-delete nil (car x))) outprops)
    (mapc (lambda (x) (org-entry-put nil (car x) (cdr x))) outprops)
    (when (not foldp)
      (org-imdb-entry-toggle-drawer 'off))))


(defun org-imdb-entry-properties-preserve-case ()
  "Get standard properties of current entry.
Extracted from `org-entry-properties' and modified to retain case,
retain + (if suitable but still concat values), and account for empty
property values."
  (org-with-point-at nil
    (org-back-to-heading-or-point-min t)
    (let ((range (org-get-property-block (point)))
          props)
      (when range
        (let ((end (cdr range)) seen-base)
          (goto-char (car range))
          (while (re-search-forward org-property-re end t)
	    (let* ((key (match-string-no-properties 2))
	           (extendp (string-match-p "\\+\\'" key))
	           (key-base (if extendp (substring key 0 -1) key))
	           (value (match-string-no-properties 3)))
	      (cond
	       ((member-ignore-case key-base org-special-properties))
	       (extendp
                (let ((old-ext (assoc-string key props t))
                      (old-base (assoc-string key-base props t)))
                  (cond
                   ((and (not old-ext)
                         (not old-base))
                    (setq props (cons (cons key value) props)))
                   (old-ext
                    (setcdr old-ext
                            (concat
                             (cdr old-ext)
                             (unless (string-empty-p (cdr old-ext)) " ")
                             value)))
                   (old-base
                    (setcdr old-base
                            (concat
                             (cdr old-base)
                             (unless (string-empty-p (cdr old-base)) " ")
                             value))))))
	       ((member-ignore-case key seen-base))
	       (t (push key seen-base)
	          (let ((p (assoc-string (concat key "+") props t)))
		    (if (not p)                        
		        (push (cons key value) props)
                      (setcar p key)
                      (setcdr p
                              (concat
                               value
                               (unless (string-empty-p value) " ")
                               (cdr p)))))))))))
      props)))


(defun org-imdb-entry-toggle-drawer (&optional arg)
  "Toggle visibility of the property drawer of entry at point.
The entry itself is always unfolded, but the drawer is only
unfolded if that is required for toggling its visbility.
When ARG is `off' always reveal the drawer.
When ARG is any other non-nil value, hide it.
When called interactively one `\\[universal-argument]' prefix
sets ARG to `t', while two set it to `off'."
  (interactive "P")
  (when (called-interactively-p 'any)
    (cond
     ((equal arg '(4)) (setq arg t))
     ((equal arg '(16)) (setq arg 'off))))
  (save-excursion
    (org-back-to-heading)
    (let ((h-folded-p (org-fold-folded-p (line-end-position)))
          (d-pos (car (org-get-property-block))))
      (when h-folded-p
        (org-fold-heading nil t))
      (when d-pos
        (goto-char d-pos)
        (left-char)
        (cond
         (h-folded-p
          (org-fold-hide-drawer-toggle (or arg 'off)))
         ((not h-folded-p)
          (org-fold-hide-drawer-toggle (or arg nil))))))))


(defun org-imdb-set-user-property (&optional prop)
  (interactive (list (completing-read "Property: "
                                      org-imdb-user-properties)))
  (org-set-property prop nil))


(defun org-imdb-edit/add-prop (prop)
  "Edit or add property PROP of current entry.
Add the property key if it doesn't exist and position point
at the property value, marking the old value if there is one."
  (interactive (list (completing-read "Property: "
                                      org-imdb-user-properties)))
  (org-imdb-entry-toggle-drawer 'off)
  (org-back-to-heading)
  (let ((old-prop (org-entry-get nil prop)))
    (if old-prop
        (progn
          (re-search-forward
           (format "^[ \t]*:%s:[ \t]*\\(.*\\)" prop))
          (set-mark-command nil)
          (goto-char (match-beginning 1)))
      (org-entry-put nil prop "")
      (re-search-forward
       (format "^[ \t]*:%s:" prop))
      (insert " "))))


(provide 'org-imdb)

;;; org-imdb.el ends here
