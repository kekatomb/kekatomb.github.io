- {{query (property type project) }}
  query-table:: true
  query-properties:: [:page :type :tags :state :description]
- {{query (property type book)}}
  query-table:: false
  query-properties:: [:page :type :tags :state :author]
- {{query (property type course)}}
  query-table:: true
  query-properties:: [:page :type :tags :state]