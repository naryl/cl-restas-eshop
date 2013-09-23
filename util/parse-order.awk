#!/usr/bin/awk -f
BEGIN {
	FS = "│" 
	ORS=""
	match(FILE, /[0-9]+/)
	FILE=substr(FILE,RSTART,RLENGTH)
	delivery=0
	total=0
}

NF==7 && !/Артикул/ {
	name[$2] = $3
	price[$2] = $4
	count[$2] = $5
}

/Доставка/ {
	delivery = $3+0
}

/Итого/ {
	total = $3+0
}

END {
	print "(:key", FILE,
		  ":delivery", delivery, ":total", total,
		  ":items (\n"
	for (art in name) {
		gsub(/[ \t]+$/, "", name[art])
		gsub(/^[ \t]+/, "", name[art])
		gsub(/"/, "\\\"", name[art])
		print "  (:article", art+0,
			  ":name \"" name[art] \
			  "\" :price", price[art]+0,
			  ":count", count[art]+0 ")\n"
	}
	print "))\n"
}
