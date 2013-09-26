#!/usr/bin/awk -f
BEGIN {
	FS = "│" 
	ORS=""
	match(FILE, /[0-9]+/)
	FILE=substr(FILE,RSTART,RLENGTH)
	delivery=0
	total=0
	order_id=0
	username=""
	addr=""
	phone=""
	email=""
	comment=""
}

NF==7 && !/Артикул/ {
	name[$2] = trim($3)
	price[$2] = $4
	count[$2] = $5
}

/Доставка/ {
	delivery = $3+0
}

/Итого/ {
	total = $3+0
}

/Номер заказа/ {
	order_id=getval($0)+0
}

/ФИО/ {
	username=trim(getval($0))
}

/Адрес доставки/ {
	addr=trim(getval($0))
}

/Контактный телефон/ {
	phone=trim(getval($0))
}

/Контактный email/ {
	email=trim(getval($0))
}

/Комментарий/ {
	comment=trim(getval($0))
}

/Дата и время заказа/ {
	date=getval($0)
	gsub(/[:-]/, " ", date)
	date=strftime("%s", mktime(date " 00"))
}

END {
	print "(:key", FILE,
		  ":delivery", delivery, ":total", total,
		  ":order-id", order_id, ":username \"" username "\"",
		  ":address \"" addr "\"", ":phone \"" phone "\"",
		  ":email \"" email "\"", ":comment \"" comment "\"",
		  ":date", date, ":items (\n"
	for (art in name) {
		print "  (:article", art+0,
			  ":name \"" name[art] \
			  "\" :price", price[art]+0,
			  ":count", count[art]+0 ")\n"
	}
	print "))\n"
}

function getval (line) {
	sub(/[^:]+: */, "", line)
	return line
}

function trim (string) {
	gsub(/[ \t]+$/, "", string)
	gsub(/^[ \t]+/, "", string)
	gsub(/"/, "\\\"", string)
	gsub(/\\/, "\\\\", string)
	return string
}

