
set cookie_file cookies.txt
set times 10000000

proc do {} {
    catch {exec curl --cookie-jar $::cookie_file http://localhost:4246/ -o /dev/null}
    catch {exec curl --cookie $::cookie_file http://localhost:4246/ -o /dev/null}
}

proc test {} {
    for {set i 0} {$i < $::times} {incr i} {
        puts "Iteration $i"
        do
    }
}
