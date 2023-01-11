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

(defcustom org-imdb-properties
  '("imdbid" "director" "rating" "votes" "genres"
    "runtime" "premiered" "type")
  "List of database columns to write as properties to Org entries.
Its elements have to be in `org-imdb-columns'."
  :type 'list)

(defcustom org-imdb-user-properties
  '("prio" "tag" "watched" "myrating")
  "List of properties whose values are manually set by the user.
These properties will be added with nil values."
  :type 'list)

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
  '("imdbid" "type" "ptitle" "otitle" "adult" "premiered"
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
	  t.premiered,
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


;;;###autoload
(defun org-imdb-update-entry ()
  "Update entry at point."
  (interactive)
  (let* ((imdbid (or (org-imdb-org-entry-get-imdbid)
		     (org-imdb-get-id)))
	 (alldata (org-imdb-get-title-data imdbid))
	 (data (mapcar (lambda (x)
			 (assoc x alldata))
		       org-imdb-properties)))
    (mapc (lambda (x)
	    (org-entry-put
	     nil
	     (funcall org-imdb-property-case-function (car x))
	     (cdr x)))
	  data)
    (when (and org-imdb-imdbid-as-link
	       (member "imdbid" org-imdb-properties))
      (org-imdb-org-entry-linkify-imdbid))
    (when org-imdb-heading-constructor
      (funcall org-imdb-heading-constructor alldata))))


(defun org-imdb-org-entry-get-imdbid ()
  "Get imdbid of entry even when it's stored as link."
  (let ((imdbid (org-entry-get nil "imdbid")))
    (if (string-prefix-p "[[" imdbid)
	(org-link-display-format imdbid)
      imdbid)))


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
	  (completing-read "Type: " org-imdb-types))
	 (query (org-imdb-get-id-query qtitle year selected-type))
	 ;; premiered = '1990' and
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
	(premiered (cdr (assoc "premiered" d))))
    (org-edit-headline
     (concat (format "%s (%s)" otitle premiered)
	     (when (> (levenshtein-distance otitle ptitle) 3)
	       (format " [%s]" ptitle))))))


;;;###autoload
(defun org-imdb-open-web ()
  "Open the IMDb website of entry at point."
  (interactive)
  (let* ((imdbid (org-imdb-org-entry-get-imdbid))
	 (url (concat "https://www.imdb.com/title/" imdbid)))
    (browse-url url)))


(provide 'org-imdb)

;;; org-imdb.el ends here
