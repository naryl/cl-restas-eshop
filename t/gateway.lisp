
(in-package eshop-test)

(deftestsuite gateway ()
  ((base-url  "http://localhost:8089/"))
  (:function
   (request (page data)
            (drakma:http-request (concatenate 'string base-url page)
                                 :method :post
                                 :content (sb-ext:string-to-octets data))))
  (:tests
   ;(products nil nil)
   (ekk
    (eshop.odm:remobj (eshop.odm:getobj 'eshop::bonuscard "1234"))
    (ensure (null (eshop.odm:getobj 'eshop::bonuscard "1234")))
    (request "gateway-ekk" (st-json:write-json-to-string (list (son "ekk" "1234" "bonuses" 0))))
    (ensure (eql 0 (eshop::bonuscard-count (eshop.odm:getobj 'eshop::bonuscard "1234"))))
    (request "gateway-ekk" (st-json:write-json-to-string (list (son "ekk" "1234" "bonuses" 1))))
    (ensure (eql 1 (eshop::bonuscard-count (eshop.odm:getobj 'eshop::bonuscard "1234")))))))
