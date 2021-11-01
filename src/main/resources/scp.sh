#! /usr/bin/expect -d

set Host [lindex $argv 0]
set User [lindex $argv 1]
set Password [lindex $argv 2]
set From [lindex $argv 3]
set To [lindex $argv 4]

spawn scp -r "$From" "$User@$Host:$To"

expect "$User@$Host's password:"
send "$Password\r"

expect eof
