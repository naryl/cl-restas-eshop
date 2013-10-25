
(in-package eshop-test)

(deftestsuite avail (eshop-avail-test)
  ((base-url  "http://localhost:8080/")
   (user "")
   (pass ""))
  (:function
   (request (page)
            (drakma:http-request (concatenate 'string base-url page)
                                 :basic-authorization (list user pass))))
  (:function
   (check-no-cache (page)
                   (trivial-timeout:with-timeout (1)
                     (multiple-value-bind (data result headers) (request page)
                       (and (eql 200 result)
                            (equal "no-cache" (assoc :pragma  headers))
                            (search "no-store" (assoc :cache-control  headers))
                            (search "no-cache" (assoc :cache-control  headers))
                            (search "must-revalidate" (assoc :cache-control  headers))
                            (search "post-check=0" (assoc :cache-control  headers))
                            (search "pre-check=0" (assoc :cache-control  headers)))))))
  (:function
   (verify-page (page)
                (trivial-timeout:with-timeout (1)
                  (ensure (eql 200
                               (second (multiple-value-list (request page))))))))
  (:tests
   (avail (verify-page ""))
   (cart (verify-page "cart"))
   (checkout (verify-page "checkout?button2"))
   (delivery (verify-page "delivery"))
   (warranty (verify-page "warranty"))
   (bonus (verify-page "bonus"))
   (corporate (verify-page "corporate"))
   (listservice (verify-page "listservice"))
   (faq (verify-page "faq"))
   (about (verify-page "about"))
   (levashovsky (verify-page "levashovsky"))
   (contacts (verify-page "contacts"))
   (vacancy (verify-page "vacancy"))
   (address (verify-page "address"))
   (warrantyservice (verify-page "warrantyservice"))
   (service (verify-page "service"))
   (dopolnitelnii-servis (verify-page "dopolnitelnii-servis"))
   (pricesc (verify-page "pricesc"))
   (articles/akcii (verify-page "articles/akcii"))
   (articles/news (verify-page "articles/news"))
   (articles/reviews (verify-page "articles/reviews"))
   (fcard (verify-page "fcard"))
   (noutbuki-i-komputery (verify-page "noutbuki-i-komputery"))
   (noutbuki (verify-page "noutbuki"))
   (product (verify-page "215543")) ; Product page
   (filter-full (verify-page "noutbuki?price-f=2000&price-t=40000&screen-size-f=&screen-size-t=&videoram-f=&videoram-t=&ram-f=&ram-t=&harddrive-f=&harddrive-t=&work-on-battery-f=&work-on-battery-t=&frequency-f=&frequency-t=&webcam-f=&webcam-t=&weight-f=&weight-t=&fullfilter=1")) ; filter
   (filter-full-2(verify-page "noutbuki?price-f=1&price-t=2&screen-size-f=&screen-size-t=&videoram-f=&videoram-t=&ram-f=&ram-t=&harddrive-f=&harddrive-t=&work-on-battery-f=&work-on-battery-t=&frequency-f=&frequency-t=&webcam-f=&webcam-t=&weight-f=&weight-t=&fullfilter=1")) ; zero-result filter
   (filter-short (verify-page "noutbuki?price-f=2000&price-t=40000&fullfilter=1")) ; filter
   (filter-short-2 (verify-page "noutbuki?price-f=1&price-t=2&fullfilter=1")) ; zero-result filter
   (model-filter (verify-page "noutbuki/noutbuki-acer-seria-aspire-one"))
   )
  (:tests
   (thanks-no-cache (check-no-cache "thanks")))
  )
