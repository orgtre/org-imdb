;;; org-imdb.el --- Interface between Org and IMDb -*- lexical-binding: t; -*-

;; Copyright (C) 2023 orgtre

;; Author: orgtre
;; Package-Requires: ((sqlite3 "0.16"))
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

(require 'sqlite3)
(require 'org)

(defgroup org-imdb nil
  "Interface between Org Mode and the IMDb public datasets."
  :group 'multimedia)

(defcustom org-imdb-db nil
  "Path to an SQLite database holding the IMDb data.
It should be created by URL `https://github.com/jojje/imdb-sqlite'
or at least follow a similar schema."
  :type 'string)

(defconst org-imdb-types '("movie" "short" "tvEpisode" "tvMiniSeries" "tvMovie" "tvPilot" "tvSeries" "tvShort" "tvSpecial" "video" "videoGame"))

(defconst org-imdb-title-query "select titles.title_id, type, primary_title, original_title, premiered, runtime_minutes, genres, rating, votes from titles left join ratings on titles.title_id = ratings.title_id where titles.title_id = '%s'")

(defconst org-imdb-crew-query "select category, group_concat(name, ', ') as names from crew inner join people on crew.person_id = people.person_id where title_id = '%s' group by category")













(provide 'org-imdb)

;;; org-imdb.el ends here
