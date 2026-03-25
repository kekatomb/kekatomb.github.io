- #+BEGIN_QUERY
  {:title [:a {:href "#/all-journals"} "View all Journals"]
   :query [:find (pull ?p [*])
           :where
           [?p :block/journal? true]]
   :result-transform (fn [result]
                       (take 5 (reverse (sort-by (fn [h] (get h :block/journal-day)) result))))
   :collapsed? false}
  #+END_QUERY
- ## Projects
	- {{query (and (property type project) (not (property template)))}}
	  query-table:: true
	  query-properties:: [:page :type :tags :state :description]
- ## Books
	- {{query (and (property type book) (not (property template)))}}
	  query-table:: false
	  query-properties:: [:page :type :tags :state :author]
- ## Courses
	- {{query (and (property type course) (not (property template)))}}
	  query-table:: true
	  query-properties:: [:page :type :tags :state]