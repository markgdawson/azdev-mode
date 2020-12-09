;;; devops-client.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 mark
;;
;; Author: Mark Dawson <http://github/M4rkD>
;; Maintainer: Mark Dawson
;; Created: March 23, 2020
;; Modified: March 23, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/M4rkD/devops-client.el
;; Package-Requires: ((emacs 26.3) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(require 'request)
(require 'ht)
(require 'dash)
(require 'cl-lib)
(require 'dom)
(require 's)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Store abstraction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun azdev/new-store ()
  (ht-create))

(defun azdev/get-data (store id)
  "Get item if from store.
If item ID is not a number, then it's probably already an item. In which case, return it directly."
  (if (numberp id)
      (ht-get store id)
    id))

(defun azdev/store-add (store key value)
  (ht-set! store key value))

(defun azdev/get-field (id data)
  (alist-get id data))

(defun azdev/store-store (store val)
  (let ((id (azdev/get-field 'id val)))
    (azdev/store-add azdev/wi-store id val)
    val))

(defun azdev/id-or-data->id (store id-or-data)
  "Return id, when given either an id or a data entry."
  (if (numberp id-or-data)
      id-or-data
    (azdev/get-field 'id id-or-data)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ewoc data abstraction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun azdev/rowref-level (row-ref)
  (plist-get row-ref 'level))

(defun azdev/rowref-data (row-ref &optional store)
  (if-let ((data (plist-get row-ref 'data)))
      data
    (if store
        (azdev/get-data store (azdev/rowref-id row-ref))
      nil)))

(defun azdev/rowref-id (row-ref &optional store)
  (plist-get row-ref 'id))

(defun azdev/rowref-create (id level &optional data)
  (if (or (not id) (numberp id))
      (list 'id id 'level level 'data data)
    (error "id should be a number or nil")))

(defun azdev/rowrefs-create-multiple (spec)
  "Spec is a list of lists, each of the sub lists
being arguments to azdev/rowref-create "
  (mapcar
   (-lambda (args)
     (apply #'azdev/rowref-create args))
   spec))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar azdev/default-project "Swansea%20Academy%20of%20Advanced%20Computing"
  "The default project.")

(defvar azdev/base-url (format "https://dev.azure.com/swansea-university/%s/_apis"
                               azdev/default-project)
  "The base url for api requests.
Note that for creating items, this needs to have a default project.
For other operations, leaving out the azdev/default-project segment seems to work.")

(defvar azdev/query-chunk-size 200
  "The maximum number of work items to fetch in one go.")

(defvar azdev/wi-store (azdev/new-store)
  "Default work item store.")

(defvar azdev/buffer "*azdev*")

(defvar azdev/token-file-path "~/.azure-devops-token")

(defvar azdev/auth-token nil)

(defvar azdev/task-display-mapping
  `(("ID" id 7 ,#'azdev/id->printed-id)
    ("Title" title 60 ,#'azdev/string-with-indent)
    ("Status" state 13 ,#'azdev/status-brackets)
    ("Updated" changed-date 13 ,(lambda (time level) (format-time-string "%Y-%m-%d" time)))
    ("Assigned To" assigned-to 20 ,#'azdev/assigned-to-string))
  "List of mappings to obtain string for each column.
Each entry is of the form:
 (column-name field-in-data column-width transform-function)
Tranform is a function which takes in the value of key field-in-data of
work item data, and returns the string to display.
The dynamic scopre variable *level* is also set in the function scope.
")

(defvar azdev/epic-feature-display-mapping
  `(("ID" id 7 ,#'azdev/id->printed-id)
    ("Title" title 60 ,#'azdev/string-with-indent)
    ("Status" state -1 ,#'azdev/status-brackets)
    ("padding" nil 34 ,#'azdev/identity))
  "List of mappings to obtain string for each column.
Each entry is of the form:
 (column-name field-in-data column-width transform-function)
Tranform is a function which takes in the value of key field-in-data of
work item data, and returns the string to display.
Negative numbers from column size denote the padding to the RHS of the string.
")

(defvar azdev/team-display-mapping
  `(("newline" nil 1 ,#'azdev/new-line)
    ("padding" nil 1 ,#'azdev/identity)
    ("Title" title 93 ,#'azdev/identity)))

(defvar azdev/map:work-item->display-string
  (list "Development Task" 'azdev/task-display-mapping
        "Admin Task" 'azdev/task-display-mapping
        "Epic" 'azdev/epic-feature-display-mapping
        "Feature" 'azdev/epic-feature-display-mapping
        'team 'azdev/team-display-mapping
        nil 'azdev/task-display-mapping)
  "Property list that specifies for each type of work item, the list of
columns to display.
Entry with key nil specifies the default entry.")

(defvar azdev/formatting-faces
  '("Development Task" (azdev-dev-task (azdev/team-bg-colour-font azdev/id-std-font azdev/format-status))
    "Admin Task" (azdev-admin-task (azdev/team-bg-colour-font azdev/id-std-font azdev/format-status))
    "Epic" (azdev-epic (azdev/team-bg-colour-font azdev/id-std-font))
    "Feature" (azdev-feature (azdev/team-bg-colour-font azdev/id-std-font))
    "Meeting" (azdev-meeting (azdev/team-bg-colour-font azdev/id-std-font))
    "Meeting attendance" (azdev-meeting (azdev/team-bg-colour-font azdev/id-std-font))
    team (azdev-team (azdev/no-font))
    )
  "Definitions of how to format rows/columns.
First element is the face to use for the row.
The second column in a list of functions to use to format each row.
Each function should take three arguments:
the work item data, the start position of the column, and the end position.")

(defvar azdev/state-colours '("Closed" "lime green"
                              "Active" "red")
  "Colours for colouring each state text.")

(defvar azdev/user-aliases '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tokens
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun azdev/load-token ()
  "Return token from azdev/toekn-file-path."
  (with-temp-buffer
    (insert-file-contents azdev/token-file-path)
    (s-trim
     (buffer-string))))

(defun azdev/read-token ()
  (setq azdev/auth-token (azdev/load-token)))

(defun default-headers (content-type)
  "Return default headers, for a given CONTENT-TYPE."
  `(("Authorization" . ,(concat "Basic " azdev/auth-token))
    ("Content-Type" . ,content-type)))

(azdev/read-token)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Communications
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun azdev/catch-request-errors (response)
  "Throw the appropriate error if RESPONSE does not have status 200.
Otherwise return the RESPONSE, unchanged."
  (if (= 200 (request-response-status-code response))
      response
    (let* ((is-json (cl-search "json" (request-response-header response "Content-Type")))
          (raw-data (request-response-data response)))

        (user-error "Error in response: %s" raw-data))))

(defun azdev/dispatch-get-request (uri)
  (azdev/--dispatch-request uri "GET" nil))

(defun azdev/dispatch-post-request (uri data)
  (azdev/--dispatch-request uri "POST" data))

(defun azdev/dispatch-patch-request (uri data)
  (azdev/--dispatch-request uri "PATCH" data))

(cl-defun azdev/--dispatch-request (uri method data &optional (content-type "application/json"))
  "Dispatch request to endpoint URI (with json parsing) and METHOD.
Return parsed json data as an alist.
Note that PATCH requests are always sent with a content type of \
application/json-patch+json,regardless of value of content-type specified.
METHOD should be a string such as \"GET\" or \"POST\""
  (let* ((url (concat azdev/base-url uri))
         (content-type (if (string= method "PATCH")
                           "application/json-patch+json"
                         content-type)))
    (message "Calling: [%S] %s" method url)
    (let* ((response (if data
                         (request
                           url
                           :type method
                           :headers (default-headers content-type)
                           :sync t
                           :data (json-encode data))
                       (request
                         url
                         :type method
                         :headers (default-headers content-type)
                         :sync t)))
           (status (request-response-status-code response)))

      ;; set the last response globally for ease of debugging
      (setq azdev/last-response response)
      (setq azdev/last-response-request (list url method data))

      (if (= status 200)
          (json-read-from-string
           (request-response-data response))
        (user-error
         (concat "Request failed: "
                 (azdev/extract-message-from-error-response response)
                 " See azdev/last-response variable for more detail."))))))

(defun azdev/handle-request-error (request)
  "Default handler for errors in the request to devops server."
  (setq azdev/last-failed-request request)
  )

(defun azdev/get-request (uri)
  "GET from URI of the current project."
  (azdev/dispatch-get-request uri))

(defun azdev/query (wiql)
  "Fetch the result of the the wiql query string given by WIQL."
  ;; wiql api used is documented at: https://docs.microsoft.com/en-us/rest/api/azure/devops/wit/wiql/query%20by%20wiql?view=azure-devops-rest-5.1
  (let ((wiql-uri "/wit/wiql?api-version=5.1"))
    (message "Wiql: %s" wiql)
    (azdev/dispatch-post-request wiql-uri `(("query" . ,wiql)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Parsing Devops Error Responses
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun azdev/extract-message-from-error-response (response)
  "Extract the message from a JSON devops error response"
  (condition-case nil
      ;; try to extract json
      (alist-get 'message
                 (json-read-from-string
                  (request-response-data
                   response)))
    (azdev/extract-message-from-html-response response)))

(defun azdev/extract-message-from-html-response (response)
  "Extract error message as first h2 in div with id=message"
  (dom-text
   (dom-child-by-tag (dom-by-id
                      (my/libxml-parse-html-response response) "message")
                     'h2)))

(defun my/libxml-parse-html-region-from-string (str)
  (with-temp-buffer
    (insert str)
    (libxml-parse-html-region (point-min) (point-max))))

(defun my/libxml-parse-html-response (resp)
 (my/libxml-parse-html-region-from-string
  (request-response-data resp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Fetch/store work items by ID
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun azdev/--fetch-work-items--chunked-list->urls (chunked-list)
  "Return a list of work item urls, given a chunked list of ids.
Given a chunked list of ids (i.e. a list of lists of ids)
return a list of urls of size equal the length of CHUNKED-LIST,
of urls to fetch that list of IDs"
  (mapcar (lambda (ids-chunk)
            (concat
            "/wit/workItems?ids="
            (mapconcat 'number-to-string ids-chunk ",")
            "&$expand=All&errorPolicy=Omit"))
          chunked-list))

(defun azdev/filter-ids-not-in-store (store ids)
  "Given a STORE and a list of IDS, return the ids not in the store."
  (seq-filter
  (lambda (key)
    (not (ht-contains? store key)))
  ids))

(defun azdev/get-relation-matching-attributes-name (relations name)
  "Get list of IDs from RELATIONS where relation type matches NAME.

NAME is either \"CHILD\" or \"PARENT\" "
  (delq nil
        (mapcar (lambda (relation)
                  (if (string= name
                              (alist-get 'name
                                          (alist-get 'attributes relation)))
                      (azdev/relation-url>id (alist-get 'url relation))
                    ))
                relations)))

(defun azdev/identity-to-name (identity)
  "Given a devops identity, extract the name (displayName)"
  (alist-get 'displayName identity))

(defvar azdev/response-field-mappings
  '((id (id))
    (title (fields System\.Title))
    (children (relations)
              (azdev/get-relation-matching-attributes-name "Child"))
    (parent (relations)
            (azdev/get-relation-matching-attributes-name "Parent"))
    (relations-raw (relations))
    (area-path (fields System\.AreaPath))
    (team (fields System\.AreaPath) (azdev/area-path->team))
    (project (fields System\.TeamProject))
    (iteration-path (fields System\.IterationPath))
    (work-item-type (fields System\.WorkItemType))
    (state (fields System\.State))
    (reason (fields System\.Reason))
    (assigned-to (fields System\.AssignedTo) (azdev/identity-to-name))
    (created-date (fields System\.CreatedDate) (date-to-time))
    (created-by (fields System\.CreatedBy) (azdev/identity-to-name))
    (changed-date (fields System\.ChangedDate) (date-to-time) )
    (changed-by (fields System\.ChangedBy) (azdev/identity-to-name))
    (comment-count (fields System\.CommentCount))
    (board-column (fields System\.BoardColumn))
    (board-columnDone (fields System\.BoardColumnDone))
    (length (fields Custom\.Length))
    (completed-work (fields Microsoft\.VSTS\.Scheduling\.CompletedWork)))
  "Mappings from local field names to the field names path in the recieved JSON.
An additional function can be provided, which is used to map remote value to local value.
A list can be provided instead of the value mapping function, in which case the first entry of
the list is the function to call, and the remaining entries are the additional arguments to pass
(after the first).")

(defun azdev/area-path->team (area-path)
  "Convert an AREA-PATH into a team name"
  (nth 1
       (s-split "\\\\" area-path)))

(defun azdev/assoc-recursive (keys alist)
  "Recursively find KEYs in ALIST."
  (while keys
    (setq alist (cdr (assoc (pop keys) alist))))
  alist)

(defun azdev/work-item-parse-field (work-item-response response-map)
  (-let* (((dest source-path func) response-map)
         (resp-value (azdev/assoc-recursive source-path work-item-response))
         (value (if (and  func resp-value (listp func))
                    (apply (car func) resp-value (cdr func))
                  resp-value)))
    (cons dest value)))

(defun azdev/work-item-parse (work-item-response)
  "Transform the downloaded json into a work item"
  (mapcar (-lambda (response-map)
            (azdev/work-item-parse-field work-item-response response-map))
          azdev/response-field-mappings))


(defun azdev/work-item-->store (store item)
  "Puts the work ITEM in the hash STORE by id, and return id."
  (let ((id (azdev/get-field 'id item)))
    (azdev/store-add store id item)
    id))

(defun azdev/fetch-work-item-data-urls (ids)
  (azdev/--fetch-work-items--chunked-list->urls
   (-partition-all
    azdev/query-chunk-size
    ids)))

(defun azdev/update-work-item! (id-or-data)
  "Fetch work item specified by ID-OR-DATA from \
the remote service and update in store.
ID-OR-DATA can be a work item id or a data alist \
with a field 'id"
  (let* ((id (azdev/id-or-data->id azdev/wi-store id-or-data))
         (items (azdev/fetch-work-items (list id)))
         (item (car items)))
    (azdev/store-add azdev/wi-store id item)
    item))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Query work items
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun azdev/extract-ids-from-query-response(wiql-query-response)
  "Extract a list of ids from the WIQL-QUERY-RESPONSE response."
  (mapcar (apply-partially 'alist-get 'id)
          (alist-get 'workItems wiql-query-response)))

(defun azdev/query-work-item-ids (wiql)
  "Fetch the ids matching a given WIQL query."
  (azdev/extract-ids-from-query-response
              (azdev/query wiql)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Specific requests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun azdev/get-projects ()
  "Fetch a list of all projects."
  (azdev/get-request "/projects"))

(defun azdev/get-teams ()
  "Fetch a list of all teams for the current project."
  (alist-get 'value
            (azdev/dispatch-get-request
              (concat "/projects/" azdev/default-project "/teams"))))

(defun azdev/get-all-work-items ()
  "Fetch a list of all teams for the current project."
  (alist-get 'value
            ((azdev/dispatch-get-request
              (concat "/projects/" azdev/default-project "/teams")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Queries as functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun azdev/query/all-work-items ()
  "Fetch all epics as a list of ids stored in STORE."
  (azdev/query-work-item-ids
   "SELECT [System.Id] FROM workitems WHERE [System.TeamProject] = 'Swansea Academy of Advanced Computing'"))

(defun azdev/query/all-epics ()
  "Fetch all epics as a list of ids stored in STORE."
  (azdev/query-work-item-ids
  "SELECT * FROM workitems WHERE [System.WorkItemType] = 'EPIC'"))

(defun azdev/query/epics-for-team (team)
  "Fetch all epics for a given TEAM into STORE.
Return ids of epics.
This function assumes that each team maps to an AreaPath."
  (azdev/query-work-item-ids
   (format "SELECT * FROM workitems WHERE [System.WorkItemType] = 'EPIC' AND \
[System.AreaPath] = 'Swansea Academy of Advanced Computing\\%s'" team)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extract relation information
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun azdev/relation-url>id (relation-url)
  "Return the id (as a number) for a RELATION-URL.
The id is extracted as the last portion of the url."
  (string-to-number
  (car
    (last
    (split-string
      relation-url
      "/")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Search and filter store
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Filtering works by functions azdev/pred/*, which create predicate functions (as closures) that can be combined with and/or.


(defun azdev/find/epics-for-given-team (store name)
  (azdev/find store (azdev/pred/and (azdev/pred/epic) (azdev/pred/team-name name))))

(defun azdev/find/team-names (store name)
  (azdev/unique-values-of-key store 'team))

(defun azdev/find/all-work-item-types ()
  (-filter #'identity
           (azdev/unique-values-of-key azdev/wi-store 'work-item-type)))

(defun azdev/find/all-area-paths (store)
  "Fetches all area paths by inspecting STORE"
 (azdev/unique-values-of-key store 'area-path))

(defun azdev/find/team-names-random-order (store)
  (require 'cookie1)

  (cookie-shuffle-vector
   (apply #'vector
          (azdev/unique-values-of-key store 'team))))

(defun azdev/find (store pred)
  "Return ids of entries for which PRED returns truthy.

PRED is a function which takes an item."
  (delete-dups
   (delq nil
         (ht-map (lambda (k v &rest rest)
                   (if (funcall pred v)
                       k
                     nil))
                 store))))

(cl-defun azdev/compute-days-since-time (work-item time &optional (key 'changed-date))
  "Utility function to compute the number of days before TIME that WORK ITEM was changed (or some other time KEY)."
  (/ (float-time
      (time-subtract
       time
       (azdev/get-field key work-item)
       )) (* 60 60 24)))

(cl-defun azdev/pred/days-since-time (days &optional (key 'changed-date))
  "Computes the number of days since the predicate function was created."
  (let ((time-now (current-time)))
    (lambda (v)
      (> days
         (azdev/compute-days-since-time v time-now key)))))

(defun azdev/pred/string-value (key string)
  (lambda (v)
    (string= (azdev/get-field key v) string)))

(defun azdev/pred/epic ()
  (azdev/pred/string-value 'work-item-type "Epic"))

(defun azdev/pred/team-name (team-name)
  (lambda (v)
    (string= (azdev/get-field 'team v) team-name)))

(defun azdev/pred/or (pred1 pred2)
  (lambda (v)
    (or (funcall pred1 v) (funcall pred2 v))))

(defun azdev/pred/and (pred1 pred2)
  (lambda (v)
    (and (funcall pred1 v) (funcall pred2 v))))

(defun azdev/unique-values-of-key (store key)
  "Fetch all unique values of field KEY in STORE."
  (delete-dups
  (ht-map (lambda (k v)
            (azdev/get-field key v))
          store)))


(azdev/unique-values-of-key azdev/wi-store 'team)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Walk the tree
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-defun azdev/walk-tree (store curr-node-id &optional (level 0) (acc nil))
  (let* ((data (azdev/get-data store curr-node-id))
         (children (azdev/get-field 'children data)))
    (if data
        (if children
          (-reduce-from (lambda (acc-child child-node-id)
                          (append acc-child
                                  (azdev/walk-tree store
                                                   child-node-id
                                                   (+ level 1))))
                        (append acc
                                (list (azdev/rowref-create curr-node-id
                                                           level))) ;; inital value
                        children)
          (list (azdev/rowref-create curr-node-id level)))
      nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Faces
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconst azdev/88-cols '((class color) (min-colors 88) (background light)))

(defface azdev-epic
  `((,azdev/88-cols
     :foreground "white"
     :height 1.0
     :background "#FF7B00"
     :weight ultra-bold)
    (default
      :background "red"
      :foreground "white"))
  "Basic face for highlighting."
  :group 'azdev-faces)

(defface azdev-feature
  `((,azdev/88-cols
     :foreground "white"
     :background "#773B93"
     :height 1.0
     :weight bold)
    (default
      :foreground "white"
      :background "magenta"))
  " Basic face for highlighting. "
:group 'azdev-faces)

(defface azdev-dev-task
  `((,azdev/88-cols
     :foreground "black"
     :background "#FBD144")
    (default
      :foreground "white"
      :background "yellow"))
       "Basic face for highlighting."
       :group 'azdev-faces)

(defface azdev-admin-task
  `((,azdev/88-cols :foreground "white"
                    :background "#0D60AB")
    (default
      :fooreground "default"))
       "Basic face for highlighting."
       :group 'azdev-faces)

(defface azdev-meeting
  `((,azdev/88-cols :foreground "black"
                    :background "cornsilk")
    (default
      :background "white"
      :foreground "black"))
  "Basic face for highlighting. "
:group 'azdev-faces)

(defface azdev-team
  `((,azdev/88-cols
     :background "dark gray"
     :foreground "white"
     :height 1.5)
    (default
      :foreground "default"
      :weight ultra-bold
      ))
       "Basic face for highlighting."
       :group 'azdev-faces)

(defface azdev-sidebar
  `((,azdev/88-cols
     :background "dark gray"
     :foreground "white")
    (default
      :foreground "default"
      :weight ultra-bold
      ))
       "Basic face for highlighting."
       :group 'azdev-faces)

(defface azdev-id-font
  `((,azdev/88-cols :height 1.0)
    (default
      :foreground "default"))
  "Basic face for highlighting."
       :group 'azdev-faces)

(defface azdev-highlight-font
  `((default
      :foreground "red"))
  "Basic face for highlighting."
  :group 'azdev-faces)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ewoc node utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun azdev/current-ewoc-node-bounds (ewoc)
  "Return a cons pair (start . end) with character position of this line
and the start of the next line (end of this line + 1)."
  (save-excursion
    (let ((start (ewoc-location (ewoc-locate ewoc)))
          (end (progn (ewoc-goto-next ewoc 1)
                      (ewoc-location (ewoc-locate ewoc)))))
      (cons start end))))

(defun azdev/ewoc-rowref-current-line (ewoc)
  "Get the ID of the work item on the current line"
  (ewoc-data
    (ewoc-locate ewoc)))

(defun azdev/ewoc-level-current-line (ewoc)
  "Get the ID of the work item on the current line"
  (azdev/rowref-level
   (azdev/ewoc-rowref-current-line ewoc)))

(defun azdev/ewoc-id-current-line (ewoc)
  "Get the ID of the work item on the current line"
  (azdev/rowref-id
   (azdev/ewoc-rowref-current-line ewoc)))

(defun azdev/ewoc-data-current-line (ewoc store)
  "Get the data of the work item on the current line"
  (azdev/get-data
   store
   (azdev/ewoc-id-current-line ewoc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Column formatting functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun azdev/id-std-font (data start end)
  "Sets font height to 1"
  (add-face-text-property start end 'azdev-id-font))

(defun azdev/format-status (data start end)
  "Function used to format the `status` column.
This uses the azdev/state-colours plist to determine the colour
to apply to the region."
  (let* ((state (azdev/get-field 'state data))
         (color (lax-plist-get azdev/state-colours state)))
    (if color
        (add-face-text-property start
                                end
                                `(:foreground ,color :weight bold)))))

(defun azdev/no-font (data start end)
  "Sets font height to 1"
  (remove-text-properties start end '(face)))

(defun azdev/team-bg-colour-font (data start end)
  (add-face-text-property start end 'azdev-sidebar))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Formatting functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun azdev/overlay-face-props (start end props)
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'face props)))

(defun azdev/get-column-widths-for-item (data)
  "For a work item DATA, return a list of the widths of each printed column.
Widths are determined by parsing azdev/get-display-mapping."
  (mapcar
   (-partial #'nth 2)
   (azdev/get-display-mapping data)))

(defun azdev/get-column-ranges-for-item (data)
  (azdev/col-widths-to-ranges
   (azdev/get-column-widths-for-item data)))

(defun azdev/col-widths-to-ranges (widths)
  (cdr
   (-reduce-from (lambda (acc it)
                   (let* ((start (cdr  (-last-item acc)))
                          (end (+ start it)))
                     (append acc (list (cons start end)))))
                 '((0 . 0))
                 widths)))

(defun azdev/lim-point-to-buffer (pos)
  (min pos
       (point-max)))


(defun azdev/format-range-from-work-item-type (data start end)
  (-let* ((wi-type (azdev/get-field 'work-item-type data))
          ((default-face column-faces) (lax-plist-get azdev/formatting-faces wi-type))
          (item-column-ranges (azdev/get-column-ranges-for-item data)))
    (if default-face
        (add-text-properties start end `(face ,default-face)))
    (if column-faces
;;;  if column-faces is defined, assume that it's a formatting
;;;  function to apply to range to format
        (-zip-with (lambda (fmt-fnc range)
                     (let* ((start-col (+ start (car range)))
                           (end-col (+ start (cdr range))))
                       (if fmt-fnc
                           (funcall fmt-fnc
                                    data
                                    (azdev/lim-point-to-buffer start-col)
                                    (azdev/lim-point-to-buffer end-col)))))
                   column-faces
                   item-column-ranges))
    ))

(defun azdev/apply-format-to-current-node (ewoc store)
  "Call format-func passing start of line and start of next line as arguments"
  (-let* (((start . end) (azdev/current-ewoc-node-bounds ewoc))
          (id (azdev/ewoc-id-current-line ewoc))
          (data (azdev/get-data store id)))
    (azdev/format-range-from-work-item-type data start end)))

(defun azdev/format-buffer (ewoc store)
  "Format the whole buffer by calling formatting function on each ewoc"
  (save-excursion
    (with-current-buffer azdev/buffer
      (ewoc-goto-node ewoc (ewoc-nth ewoc 0))
      (azdev/apply-format-to-current-node ewoc store)
      (while (azdev/ewoc-next-line-or-nil ewoc)
        (azdev/apply-format-to-current-node ewoc store)))))

(defun azdev/ewoc-next-line-or-nil (ewoc)
  (let ((curr (point)))
    (ewoc-goto-next ewoc 1)
    ;; Skip lines where id is nil, usually titles etc
    (while (not (azdev/ewoc-id-current-line ewoc))
      (ewoc-goto-next ewoc 1))
    (if (= curr (point))
        nil
      t)))

(defun azdev/ewoc-prev-line-or-nil (ewoc)
  (let ((curr (point)))
    (ewoc-goto-prev ewoc 1)
    ;; Skip lines where id is nil, usually titles etc
    (while (not (azdev/ewoc-id-current-line ewoc))
      (ewoc-goto-prev ewoc 1))
    (if (= curr (point))
        nil
      t)))

(defun azdev/ewoc-next-team-or-nil (ewoc)
  (let ((curr (point)))
    ;; Skip lines until id is nil (i.e. a title)
    (catch 'break
      (while (azdev/ewoc-id-current-line ewoc)
        (ewoc-goto-next ewoc 1)

        ;; if we're at the end, move back to the original point (i.e no action)
        (when  (= (count-lines (point) (point-max)) 0)
          (goto-char curr)
          (throw 'break 1)))
      (ewoc-goto-next ewoc 1))
    (if (= curr (point))
        nil
      t)))

(defun azdev/ewoc-prev-team-or-nil (ewoc)
  (let ((curr (point)))
    ;; Skip lines until id is nil (i.e. a title)
    (while (azdev/ewoc-id-current-line ewoc)
      (ewoc-goto-prev ewoc 1))
    (ewoc-goto-prev ewoc 1)
    (if (= curr (point))
        nil
      t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Filtering functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun azdev/filter-rowref (filter-func)
  "Return results of applying predicate FILTER-FUNC to row reference ROWREF.
FILTER-FUNC should take two arguments, the rowref data and the rowref level "
  (lambda (rowref)
    (let ((data
           (azdev/rowref-data rowref azdev/wi-store))
          (level
           (azdev/rowref-level rowref)))
      (funcall filter-func data level))))

(defun azdev/filter-rowrefs (rowrefs filter-func)
  "Filter list of row references, ROWREFS, using the predicate function FILTER-FUNC.
FILTER-FUNC takes two arguments, the rowref data and the rowref level "
  (-filter
   (azdev/filter-rowref filter-func)
   rowrefs))

(defun filter-nothing (data level)
  "Show all work items (based on work item DATA and LEVEL)."
  t)

(defun filter-only-not-closed (data level)
  "Show only items that are not closed (based on work item DATA and LEVEL)."
  (let* ((id (azdev/get-field 'id data))
         (label (azdev/get-field 'title data))
         (wi-type (azdev/get-field 'work-item-type data))
         (changed-time (azdev/get-field 'changed-date data))
         (wi-state (azdev/get-field 'state data))
         (assigned-to (azdev/get-field 'assigned-to data)))
    (not  (string= wi-state "Closed"))))

(defun filter-not-closed-or-recent-activity (data level)
  "Show only items that are not closed (based on work item DATA and LEVEL)."
  (let* ((id (azdev/get-field 'id data))
         (label (azdev/get-field 'title data))
         (wi-type (azdev/get-field 'work-item-type data))
         (changed-time (azdev/get-field 'changed-date data))
         (wi-state (azdev/get-field 'state data))
         (assigned-to (azdev/get-field 'assigned-to data))
         (check-time (azdev/pred/days-since-time 14)))
    (or (not  (string= wi-state "Closed"))
        (funcall check-time data))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Printing utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun azdev/string-with-indent (title level)
  (concat
   (s-repeat (azdev/indent-length level) " ")
   title))

(defun azdev/id->printed-id (id level)
  (concat " "
          (number-to-string id)))

(defun azdev/status-brackets (status level)
  (concat "[" (s-pad-right 9 " " status) "]"))

(defun azdev/new-line (id level)
  "Inserts a new line"
  "\n")

(defun azdev/identity (id level)
  id)

(defun azdev/assigned-to-string (name level)
  (or (lax-plist-get azdev/user-aliases name) name ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Printing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun azdev/indent-length (level)
  (+ 1 (* 4 level)))

(defun azdev/team-work-item-rowref (store team-name)
  "Get (level . id) cons pairs for items to show."
  (mapcan (lambda (epic-id)
            (azdev/walk-tree store epic-id))
          (azdev/find/epics-for-given-team store team-name)))

(defun azdev/get-display-mapping (data)
  "Given a work item data, return specification of the columns of that work item."
  (let* ((wi-type (azdev/get-field 'work-item-type data))
         (val (or (lax-plist-get azdev/map:work-item->display-string
                                 wi-type)
                  (lax-plist-get azdev/map:work-item->display-string
                                 nil))))
    (cond ((symbolp val) (eval val))
          val)))

(defun azdev/add-text-props-to-string (props str)
  (let ((len (length str)))
    (add-text-properties 0 len props str)
    str))

(cl-defun azdev/string-for-work-item (data &optional (level 0))
  "For a given data entry, return the values of columns as a vector.
The way to obtain columns is defined in azdev/string-for-task-display-mapping."
          (let ((display-mapping (azdev/get-display-mapping data)))
            (apply #'concat
                   (mapcar
                    (-lambda ((col-name key length func))
                      (let* ((result (funcall func
                                             (azdev/get-field key data)
                                             level))
                            (str-length (if (< length 1)
                                            (- (length result) length)
                                      length)))
                        (s-truncate str-length
                                    (s-pad-right str-length " "
                                                 result))))
                    display-mapping))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Fetching and storing work items
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun azdev/fetch-work-items (ids)
  "Fetch data for each items from IDS in chunks."
  (mapcan
   (lambda (url)
     ;; loop over urls
     (let* ((response (azdev/get-request url))
            (values (azdev/get-field 'value response)))
       (mapcar
        (lambda (value)
          (azdev/work-item-parse value))
        values)))
   (azdev/fetch-work-item-data-urls ids)))

(defun azdev/fetch-and-set-work-items (store ids)
  "Fetch all work items specified in IDS into STORE."
  (dolist (item (azdev/fetch-work-items ids))
        (azdev/store/set-item store item)))

(defun azdev/store/set-item (store item)
  "Add an ITEM into STORE."
  (azdev/store-add azdev/wi-store (azdev/get-field 'id item) item))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Remote URLs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun azdev/id->relation-url (id)
  "Return the URL of a work item for item ID-OR-DATA.
ID-OR-DATA is either an item ID or an alist containing \
field id."
  (concat
   "https://dev.azure.com/swansea-university/Swansea Academy of Advanced Computing/_workitems/edit/"
   (number-to-string id)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; High-level functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun azdev/fetch-and-set-all-items (store)
  "Fetch all work items and set them in STORE (e.g. when starting up)."
  (azdev/fetch-and-set-work-items
   store
   (azdev/query/all-work-items)))

(defun azdev/fetch-and-set-all-new-items (store)
  "Fetch all items that are not already in STORE, and set them in STORE."
(azdev/fetch-and-set-work-items
 store
 (azdev/filter-ids-not-in-store store
                                    (azdev/query/all-work-items))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Creating ewoc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun azdev/ewoc-printer (row-ref)
  "TODO: the store is accessed directly here"
  (let* ((level (azdev/rowref-level row-ref))
         (data (azdev/rowref-data row-ref azdev/wi-store))
         (start (point)))

    (insert (concat (azdev/string-for-work-item data level)
                    "\n"))

    (azdev/format-range-from-work-item-type data start (point))))

(defun azdev/add-team-items-to-ewoc (ewoc store teams)
  "Print the lines for all TEAMS using insert."
  (mapc
   (lambda (team-name)
     (ewoc-enter-last ewoc
                      (azdev/rowref-create
                       nil
                       0
                       `((title . ,team-name)
                         (work-item-type . team))))
     (azdev/add-items-to-ewoc
      ewoc
      (azdev/filter-rowrefs
       (azdev/team-work-item-rowref store team-name)
       #'filter-not-closed-or-recent-activity)))
   teams))

(cl-defun azdev/add-items-to-ewoc (ewoc rowref)
  "Prints the provrowref"
  (mapc
   (lambda (rowref)
     (ewoc-enter-last ewoc rowref))
   rowref))

(defun azdev/create-teams-ewoc! (store teams)
  (with-current-buffer azdev/buffer
    (erase-buffer)
    (let ((ewoc
           (ewoc-create
            #'azdev/ewoc-printer
            nil
            nil
            t)))
      (azdev/add-team-items-to-ewoc ewoc store teams)
      ewoc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Finding ewoc node and id
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun azdev/ewoc-all-row-refs (ewoc)
  "Returns a list of all row refs in EWOC, in the order they appear in the buffer."
   (ewoc-collect ewoc (lambda (v) t)))

(defun azdev/ewoc-all-ids (ewoc)
  "Returns a list of all ids in EWOC (including nils) in the order they appear in the buffer."
  (mapcar
   #'azdev/rowref-id
   (azdev/ewoc-all-row-refs ewoc)))

(defun azdev/ewoc-get-node-by-id (ewoc id)
  (ewoc-nth ewoc (-elem-index id (azdev/ewoc-all-ids azdev/wi-ewoc))))

(defun azdev/ewoc-goto-by-id (ewoc id)
  "Moves point to the node of EWOC identitified by ID."
  (pop-to-buffer azdev/buffer)
  (ewoc-goto-node ewoc
                  (azdev/ewoc-get-node-by-id ewoc id))
  (point))

(defun azdev/ewoc-delete-by-id (ewoc id)
  "TODO: this could fail, leaving buffer in a readable state."
  (with-current-buffer azdev/buffer
    (let ((inhibit-read-only t))
      (ewoc-delete ewoc
                   (azdev/ewoc-get-node-by-id ewoc id)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Updating entries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun azdev/apply-updates-current-ewoc-item (ewoc store update-item-f)
  "Update the current item from EWOC in STORE and remotely using \
function UPDATE-ITEM-F.
UPDATE-ITEM-F take the item data, and return a list of changes."
  (-let* ((node (ewoc-locate ewoc))
          (rowref (azdev/ewoc-rowref-current-line ewoc))
          (item (azdev/rowref-data rowref store))
          (item-id (azdev/rowref-id rowref))
          (changes (funcall update-item-f item)))
    (when changes
      (azdev/store-add store item-id
               (azdev/apply-server-changes item-id changes))
      (ewoc-invalidate ewoc node))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Update server
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun azdev/source-path->devops-path (source-path)
  "Convert a source path specified as a SOURCE-PATH such as \
\(field1 field2 field3 ...\) to a string field1/field2/field3/.."
  (concat "/"
          (s-join "/"
                  (mapcar
                   #'symbol-name
                   source-path))))

(defun azdev/local-key->devops-path (local-key)
  (azdev/source-path->devops-path
   (car
    (azdev/get-field local-key azdev/response-field-mappings))))

(defun azdev/spec-to-update-remote (field value operation)
  "Return DevOps API specification of an update to FIELD to VALUE with OPERATION.
Field is either a symbol specifying the local key  of the
field (e.g. title), or a string specifying the remote key
(e.g. fields/System.AssignedTo).
OPERATION is one of \"replace\", \"delete\" etc.
"
  `((op . ,operation)
    (path . ,(if (stringp field)
                 field
               (azdev/local-key->devops-path field)))
    (value . ,value)))

(defun azdev/multi-specs-to-update-remote (changes)
  "Returns a vector of multiple specs from list of CHANGES.
CHANGES is of the form:
  '((field new-value operation)
    (another-field new-value operation))
where fields are specified with either their local representation as a symbol
(e.g. title) or a full remote path as a string (e.g. \"fields/System.AssignedTo \" ).
If operation is not provided, it defaults to replace"
  (apply #'vector
         (mapcar
          (-lambda ((field value operation))
            (azdev/spec-to-update-remote field value (or operation "replace")))
          changes)))

(cl-defun azdev/apply-server-changes (work-item-id changes &optional (method "PATCH") (content-type "application/json-patch+json"))
  "Apply CHANGES to work item with WORK-ITEM-ID.
Return the new parsed work item.
CHANGES is as specified in azdev/multi-spacs-to-update-remote"
  (azdev/work-item-parse
   (azdev/--dispatch-request (format
                              "/wit/workitems/%s?api-version=5.1&$expand=All&bypassRules=true"
                              work-item-id)
                             method
                             (azdev/multi-specs-to-update-remote changes)
                             content-type)))

(defun azdev/spec-area-path (area-path)
  "Devops JSON specification for operation to set the AREA PATH."
  (if (-contains? (azdev/find/all-area-paths azdev/wi-store)
                  area-path)
      `(area-path ,area-path "add")
    (error "Invalid area path " area-path)))


(defun azdev/spec-add-parent (parent-id)
  "Devops JSON specification for operation to add parent link."
  `("/relations/-"
    ((rel . "System.LinkTypes.Hierarchy-Reverse")
     (url .  ,(azdev/id->relation-url parent-id)))
    "add"))

(defun azdev/spec-add-child (child-id)
  "Devops JSON specification for operation to add child link."
  `("/relations/-"
    ((rel . "System.LinkTypes.Hierarchy-Forward")
     (url .  ,(azdev/id->relation-url child-id)))
    "add"))

(defun azdev/delete-work-item (store id-or-data)
  "Deleted the work item ID-OR-DATA from server and STORE."
  (let* ((id (azdev/id-or-data->id store id-or-data))
        (item (azdev/apply-server-changes
               id
               nil
               "DELETE")))
    (ht-remove! store id)
    item))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Interactive functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar team-order nil)

(defun azdev/draw ()
  (interactive)

  (unless team-order
    (message "team-order variable should be set to vector of team names."))

  (pop-to-buffer azdev/buffer)

  (azdev-mode)

  (let ((inhibit-read-only t))
    (setq-local azdev/wi-ewoc
                (azdev/create-teams-ewoc! azdev/wi-store team-order))))

(defun azdev/randomise-team-order ()
  (interactive)

  (require 'cookie1)

  (setq team-order
        (cookie-shuffle-vector
         team-order)))

(defun azdev ()
  (interactive)

  ;; reset the store
  (setq azdev/wi-store (azdev/new-store))

  (azdev/fetch-and-set-all-items azdev/wi-store)

  (azdev/randomise-team-order)

  (azdev/draw))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Update definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun azdev/update-item/set-state-new (item)
  '((state "New")))

(defun azdev/update-item/set-state-closed (item)
  '((state "Closed")))

(defun azdev/update-item/set-state-active (item)
  '((state "Active")))

(defun azdev/update-item/set-title (item)
  `((title ,(read-from-minibuffer "Title: "
                                    (azdev/get-field 'title item)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Interactive functions to bind to keys
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun azdev/quit ()
  (interactive)
  (kill-buffer azdev/buffer))

(defun azdev/next-entry ()
  (interactive)
  (azdev/ewoc-next-line-or-nil azdev/wi-ewoc))

(defun azdev/next-team ()
  (interactive)
  (azdev/ewoc-next-team-or-nil azdev/wi-ewoc))

(defun azdev/prev-team ()
  (interactive)
  (azdev/ewoc-prev-team-or-nil azdev/wi-ewoc))

(defun azdev/prev-entry ()
  (interactive)
  (azdev/ewoc-prev-line-or-nil azdev/wi-ewoc))

(defun azdev/set-current-item-state--new ()
  (interactive)
  (azdev/apply-updates-current-ewoc-item
   azdev/wi-ewoc
   azdev/wi-store
   #'azdev/update-item/set-state-new))

(defun azdev/set-current-item-state--closed ()
  (interactive)
  (azdev/apply-updates-current-ewoc-item
   azdev/wi-ewoc
   azdev/wi-store
   #'azdev/update-item/set-state-closed))


(defun azdev/set-current-item-state--active ()
  (interactive)
  (azdev/apply-updates-current-ewoc-item
   azdev/wi-ewoc
   azdev/wi-store
   #'azdev/update-item/set-state-active))

(defun azdev/set-current-item-state--cycle ()
  (interactive)
  (let* ((data
          (azdev/ewoc-data-current-line azdev/wi-ewoc azdev/wi-store))
         (state (azdev/get-field 'state data))
         (mappings (list "New" #'azdev/update-item/set-state-active
                         "Active" #'azdev/update-item/set-state-closed
                         "Closed" #'azdev/update-item/set-state-new))
         (next (lax-plist-get mappings state)))
    (azdev/apply-updates-current-ewoc-item
     azdev/wi-ewoc
     azdev/wi-store
     next)))

(defun azdev/set-current-item-title ()
  (interactive)
  (azdev/apply-updates-current-ewoc-item
   azdev/wi-ewoc
   azdev/wi-store
   #'azdev/update-item/set-title))

(defun azdev/visit-current-item-www ()
  (interactive)
  (let* ((item-id (azdev/ewoc-id-current-line azdev/wi-ewoc))
         (url (azdev/id->relation-url item-id)))
    (message (concat "Visiting: " url))
    (browse-url url)))

(defun azdev/fetch-current-id ()
  (interactive)
  (message (number-to-string (azdev/ewoc-id-current-lined azdev/wi-ewoc))))

(defun azdev/print-to-pdf ()
  (interactive)
  (my/pdf-print-buffer-with-faces "~/Desktop/devops.devops"))

(defun azdev/add-item (title)
  (interactive "sTitle: \n")

  (if (< 0 (length title))
      (-let* ((parent-id (azdev/pick-work-item azdev/wi-ewoc "Parent: "))
              (type (azdev/pick-work-item-type))
              (item (azdev/create-new-child-item azdev/wi-store
                                                 title
                                                 parent-id
                                                 type))
              (child-id (azdev/id-or-data->id azdev/wi-store item))
              (parent-node
               (azdev/ewoc-get-node-by-id azdev/wi-ewoc parent-id))
              (rowref  (ewoc-data parent-node))
              (id (azdev/rowref-id rowref))
              (level (azdev/rowref-level rowref)))

        (ewoc-enter-after
         azdev/wi-ewoc
         parent-node
         (azdev/rowref-create
          child-id (+ 1 level))))))

(defun azdev/remove-item ()
  "Select a work item to delete."
  (interactive)
  (let ((id (azdev/pick-work-item azdev/wi-ewoc "Item to delete: ")))
    (when (y-or-n-p (format "Delete: %s" id))
        (azdev/delete-work-item azdev/wi-store id)
        (azdev/ewoc-delete-by-id azdev/wi-ewoc id))))

(defun azdev/htmlize-buffer-and-open (filename)
  (interactive)
  (azdev/generate-html filename)
  (if (called-interactively-p 'interactive)
      (browse-url-of-file filename))
  filename)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Generate HTML
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun azdev/generate-and-upload-code ()
  (azdev)
  (with-current-buffer azdev/buffer
    (azdev/htmlize-and-upload)))

(cl-defun azdev/insert-timestamp (&optional (prefix ""))
  (let ((inhibit-read-only t))
    (goto-char (point-min))
    (kill-line)
    (insert (format-time-string "%Y-%m-%dT%H:%M:%S\n"))))

(defun azdev/generate-html (filename)
  (with-current-buffer azdev/buffer
    (azdev/insert-timestamp "Generated: ")
    (let ((inhibit-read-only t)
          (buffer (htmlize-buffer)))
      (with-current-buffer buffer
        (azdev/--html-add-hyperlinks)
        (write-file filename))
      buffer)))

(defun azdev/--html-add-hyperlinks ()
  (goto-char (point-min))
  (let ((re "\\(.*?azdev-sidebar\">\s*\\)\\([0-9]*\\)\\(.*\\)")
        (rep
         "<a target=\"_blank\" href=\"https:\/\/dev.azure.com\/swansea-university\/Swansea%20Academy%20of%20Advanced%20Computing\/_workitems\/edit\/\\2\">\\1\\2\\3<\/a>"))
    (while (re-search-forward re nil t)
      (replace-match rep))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Creating nodes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun azdev/create-blank-work-item (type title)
  "Return a newly create work item from the server \
of given TYPE with specified TITLE."
  (if (-contains? (azdev/find/all-work-item-types)
                  type)
      (azdev/apply-server-changes (s-replace " " "%20" (concat "$" type))
                                  `((title ,title "add")))
    (error "Invalid Type" type)))

(defun azdev/attach-child-to-parent (child parent)
  "Takes two data items, and connectes them, also setting
child to be in the area path of parent.
Return the new child."
  (let ((child-id (azdev/get-field 'id child))
        (parent-id (azdev/get-field 'id parent))
        (parent-area-path (azdev/get-field 'area-path parent)))
    (azdev/apply-server-changes child-id
                                (list (azdev/spec-add-parent parent-id)
                                      (azdev/spec-area-path parent-area-path))
                                "PATCH")))

(defun azdev/assign-child-a-parent (child parent)
  "Takes two data items, and connectes them, also setting
child to be in the area path of parent.
Return a cons cell of child and parent.
Unlike the `attach-child-to-parent', this requires
in two API calls. "
  (let ((child-id (azdev/get-field 'id child))
        (parent-id (azdev/get-field 'id parent))
        (parent-area-path (azdev/get-field 'area-path parent))
        ;; add child to parent
        (child
         (azdev/apply-server-changes parent-id
                                     (list (azdev/spec-add-child child-id))
                                     "PATCH"))
        (parent
         ;; set parent to have the same path as child
         (azdev/apply-server-changes child-id
                                     (list (azdev/spec-area-path parent-area-path))
                                     "PATCH")))
    (cons child parent)))

(defun azdev/create-new-child-item (store title parent type)
  "Create and add to STORE a new item which TITLE, \
PARENT and TYPE.
Parent is updated before creating child, in order to ensure
the correct area path for child."
  (azdev/store-store
   store
   (azdev/attach-child-to-parent
    (azdev/create-blank-work-item type title)
    (azdev/update-work-item! parent))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ivy completion functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun azdev/pick-work-item-type ()
  (ivy-read "Feature: " (azdev/find/all-work-item-types)))

(cl-defun azdev/pick-work-item (ewoc &optional (str "Pick work item: "))
  (string-to-number
   (car
    (s-split "|"
             (ivy-read str
                       (delq nil
                             (ht-map (lambda (key val)
                                       (if (numberp key)
                                           (cons (format "%s |  %s" key (azdev/get-field 'title val))
                                                 key)))
                                     azdev/wi-store))
                       :initial-input (number-to-string (azdev/ewoc-id-current-line ewoc)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Major mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun set-local-keys (keymap key-defns)
  (mapcar (-lambda ((key-str . key-func))
            (define-key keymap (kbd key-str) key-func))
	  key-defns)
  nil)


(defun azdev/setup-keybindings ()
  (set-local-keys azdev-mode-map
		  '(
		    ;; Movement
		    ("C-n" . azdev/next-entry)
		    ("C-p" . azdev/prev-entry)
		    ("M-n" . azdev/next-team)
		    ("M-p" . azdev/prev-team)
		    ;; Add/remove items
		    ("C-c n" . azdev/add-item)
		    ("C-c d" . axdev/remove-item)
		    ;; Edit keymap
		    ("<tab>" . azdev/set-current-item-state--cycle)
		    ("C-c e a" . azdev/set-current-item-state--active)
		    ("C-c e n" . azdev/set-current-item-state--new)
		    ("C-c e c" . azdev/set-current-item-state--closed)
		    ("C-c e t" . azdev/set-current-item-title)
		    ;; Misc
		    ("C-c C-h" . adzev/htmlize-buffer-and-open)
		    ("C-c C-i" . azdev/fetch-current-id)
		    ("C-c C-v" . azdev/visit-current-item-www)
		    ("C-c C-p" . azdev/print-to-pdf))))

(define-derived-mode azdev-mode special-mode "Azure Devops Interaction Mode"
  "major mode for interacting with azure devops. "
  (setq-local hl-line-face 'azdev-highlight-font)
  (hl-line-mode 1)
  (azdev/setup-keybindings))

(provide 'azdev)

;;; devops.el ends here
