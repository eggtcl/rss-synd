# -*- tab-width: 4; indent-tabs-mode: t; -*-
# rss-synd.tcl -- 0.5.1
#
#   Highly configurable asynchronous RSS & Atom feed reader for Eggdrops 
#     written in TCL. Supports multiple feeds, gzip compressed feeds,
#     automatically messaging channels with updates at set intervals,
#     custom private/channel triggers and more.
#
# Copyright (c) 2011 Andrew Scott, HM2K
#
# Name: RSS & Atom Syndication Script for Eggdrop
# Author: Andrew Scott <andrew.scott@wizzer-it.com>
# Author: HM2K <irc@hm2k.org>
# License: See LICENSE file
# Link: http://code.google.com/p/rss-synd/
# Tags: rss, atom, syndication
# Updated: 05-Jan-2011
#
###Usage
# See README file
#
###Revisions
# See HISTORY file

#
# Include Settings
#
if {[catch {source scripts/rss-synd-settings.tcl} err]} {
  putlog "Error: Could not load 'rss-synd-settings.tcl file.'";
}

proc ::rss-synd::init {args} {
	variable rss
	variable default
	variable version
	variable packages

	set version(number)	0.5.1
	set version(date)	"2012-02-27"

	package require http
	set packages(base64) [catch {package require base64}]; # http auth
	set packages(tls) [catch {package require tls}]; # https
	set packages(trf) [catch {package require Trf}]; # gzip compression

	foreach feed [array names rss] {
		array set tmp $default
		array set tmp $rss($feed)

		set required [list "announce-output" "trigger-output" "max-depth" "update-interval" "timeout" "channels" "output" "user-agent" "url" "database" "trigger-type" "announce-type"]
		foreach {key value} [array get tmp] {
			if {[set ptr [lsearch -exact $required $key]] >= 0} {
				set required [lreplace $required $ptr $ptr]
			}
		}

		if {[llength $required] == 0} {
			regsub -nocase -all -- {@@feedid@@} $tmp(trigger) $feed tmp(trigger)

			set ulist [regexp -nocase -inline -- {(http(?:s?))://(?:(.[^:]+:.[^@]+)?)(?:@?)(.*)} $tmp(url)]

			if {[llength $ulist] == 0} {
				putlog "\002RSS Error\002: Unable to parse URL, Invalid format for feed \"$feed\"."
				unset rss($feed)
				continue
			}

			set tmp(url) "[lindex $ulist 1]://[lindex $ulist 3]"

			if {[lindex $ulist 1] == "https"} {
				if {$packages(tls) != 0} {
					putlog "\002RSS Error\002: Unable to find tls package required for https, unloaded feed \"$feed\"."
					unset rss($feed)
					continue
				}

				::http::register https 443 ::tls::socket
			}

			if {(![info exists tmp(url-auth)]) || ($tmp(url-auth) == "")} {
				set tmp(url-auth) ""

				if {[lindex $ulist 2] != ""} {
					if {$packages(base64) != 0} {
						putlog "\002RSS Error\002: Unable to find base64 package required for http authentication, unloaded feed \"$feed\"."
						unset rss($feed)
						continue
					}

					set tmp(url-auth) [::base64::encode [lindex $ulist 2]]
				}
			}

			if {[regexp {^[0123]{1}:[0123]{1}$} $tmp(trigger-type)] != 1} {
				putlog "\002RSS Error\002: Invalid 'trigger-type' syntax for feed \"$feed\"."
				unset rss($feed)
				continue
			}

			set tmp(trigger-type) [split $tmp(trigger-type) ":"]

			if {([info exists tmp(charset)]) && ([lsearch -exact [encoding names] [string tolower $tmp(charset)]] < 0)} {
				putlog "\002RSS Error\002: Unable to load feed \"$feed\", unknown encoding \"$tmp(charset)\"."
				unset rss($feed)
				continue
			}
			
			if {([info exists tmp(feedencoding)]) && ([lsearch -exact [encoding names] [string tolower $tmp(feedencoding)]] < 0)} {
				putlog "\002RSS Error\002: Unable to load feed \"$feed\", unknown feedencoding \"$tmp(feedencoding)\"."
				unset rss($feed)
				continue
			}

			set tmp(updated) 0
			if {([file exists $tmp(database)]) && ([set mtime [file mtime $tmp(database)]] < [unixtime])} {
				set tmp(updated) [file mtime $tmp(database)]
			}

			set rss($feed) [array get tmp]
		} else {
			putlog "\002RSS Error\002: Unable to load feed \"$feed\", missing one or more required settings. \"[join $required ", "]\""
			unset rss($feed)
		}

		unset tmp
	}

	bind evnt -|- prerehash [namespace current]::deinit
	bind time -|- {* * * * *} [namespace current]::feed_get
	bind pubm -|- {* *} [namespace current]::trigger
	bind msgm -|- {*} [namespace current]::trigger

	putlog "\002RSS Syndication Script v$version(number)\002 ($version(date)): Loaded."
}

proc ::rss-synd::deinit {args} {
	catch {unbind evnt -|- prerehash [namespace current]::deinit}
	catch {unbind time -|- {* * * * *} [namespace current]::feed_get}
	catch {unbind pubm -|- {* *} [namespace current]::trigger}
	catch {unbind msgm -|- {*} [namespace current]::trigger}

	foreach child [namespace children] {
		catch {[set child]::deinit}
	}

	namespace delete [namespace current]
}

#
# Trigger Function
##

proc ::rss-synd::trigger {nick user handle args} {
	variable rss
	variable default

	set i 0
	set chan ""
	if {[llength $args] == 2} {
		set chan [lindex $args 0]
		incr i
	}
	set text [lindex $args $i]

	array set tmp $default

	if {[info exists tmp(trigger)]} {
		regsub -all -- {@@(.*?)@@} $tmp(trigger) "" tmp_trigger
		set tmp_trigger [string trimright $tmp_trigger]

		if {[string equal -nocase $text $tmp_trigger]} {
			set list_feeds [list]
		}
	}

	unset -nocomplain tmp tmp_trigger

	foreach name [array names rss] {
		array set feed $rss($name)

		if {(![info exists list_feeds]) && \
		    ([string equal -nocase $text $feed(trigger)])} {
			if {(![[namespace current]::check_channel $feed(channels) $chan]) && \
			    ([string length $chan] != 0)} {
				continue
			}

			set feed(nick) $nick

			if {$chan != ""} {
				set feed(type) [lindex $feed(trigger-type) 0]
				set feed(channels) $chan
			} else {
				set feed(type) [lindex $feed(trigger-type) 1]
				set feed(channels) ""
			}

			if {[catch {set data [[namespace current]::feed_read]} error] == 0} {
				if {![[namespace current]::feed_info $data]} {
					putlog "\002RSS Error\002: Invalid feed database file format ($feed(database))!"
					return
				}

				if {$feed(trigger-output) > 0} {
					set feed(announce-output) $feed(trigger-output)

					[namespace current]::feed_output $data
				}
			} else {
				putlog "\002RSS Warning\002: $error."
			}
		} elseif {[info exists list_feeds]} {
			if {$chan != ""} {
				# triggered from a channel
				if {[[namespace current]::check_channel $feed(channels) $chan]} {
					lappend list_feeds $feed(trigger)
				}
			} else {
				# triggered from a privmsg
				foreach tmp_chan $feed(channels) {
					if {([catch {botonchan $tmp_chan}] == 0) && \
					    ([onchan $nick $tmp_chan])} {
						lappend list_feeds $feed(trigger)
						continue
					}
				}
			}
		}
	}

	if {[info exists list_feeds]} {
		if {[llength $list_feeds] == 0} {
			lappend list_feeds "None"
		}

		lappend list_msgs "Available feeds: [join $list_feeds ", "]."

		if {$chan != ""} {
			set list_type [lindex $feed(trigger-type) 0]
			set list_targets $chan
		} else {
			set list_type [lindex $feed(trigger-type) 1]
			set list_targets ""
		}

		[namespace current]::feed_msg $list_type $list_msgs list_targets $nick
	}
}

#
# Feed Retrieving Functions
##

proc ::rss-synd::feed_get {args} {
	variable rss

	set i 0
	foreach name [array names rss] {
		if {$i == 3} { break }

		array set feed $rss($name)

		if {$feed(updated) <= [expr { [unixtime] - ($feed(update-interval) * 60) }]} {
			::http::config -useragent $feed(user-agent)

			set feed(type) $feed(announce-type)
			set feed(headers) [list]

			if {$feed(url-auth) != ""} {
				lappend feed(headers) "Authorization" "Basic $feed(url-auth)"
			}

			if {([info exists feed(enable-gzip)]) && ($feed(enable-gzip) == 1)} {
				lappend feed(headers) "Accept-Encoding" "gzip"
			}

			catch {::http::geturl "$feed(url)" -command "[namespace current]::feed_callback {[array get feed] depth 0}" -timeout $feed(timeout) -headers $feed(headers)} debug

			set feed(updated) [unixtime]
			set rss($name) [array get feed]
			incr i
		}

		unset feed
	}
}

proc ::rss-synd::feed_callback {feedlist args} {
	set token [lindex $args end]
	array set feed $feedlist

	upvar 0 $token state

	if {[set status $state(status)] != "ok"} {
		if {$status == "error"} { set status $state(error) }
		putlog "\002RSS HTTP Error\002: $state(url) (State: $status)"
		::http::cleanup $token
		return 1
	}

	array set meta $state(meta)

	if {([::http::ncode $token] == 302) || ([::http::ncode $token] == 301)} {
		set feed(depth) [expr {$feed(depth) + 1 }]

		if {$feed(depth) < $feed(max-depth)} {
			catch {::http::geturl "$meta(Location)" -command "[namespace current]::feed_callback {$feedlist}" -timeout $feed(timeout) -headers $feed(headers)}
		} else {
			putlog "\002RSS HTTP Error\002: $state(url) (State: timeout, max refer limit reached)"
		}

		::http::cleanup $token
		return 1
	} elseif {[::http::ncode $token] != 200} {
		putlog "\002RSS HTTP Error\002: $state(url) ($state(http))"
		::http::cleanup $token
		return 1
	}

	set data [::http::data $token]
	
	if {[info exists feed(feedencoding)]} {
		set data [encoding convertfrom [string tolower $feed(feedencoding)] $data]
	}

	if {[info exists feed(charset)]} {
		if {[string tolower $feed(charset)]) == "utf-8" && [is_utf8_patched]} {
			#do nothing, already utf-8
		} else {
			set data [encoding convertto [string tolower $feed(charset)] $data]
		}
	}

	if {([info exists meta(Content-Encoding)]) && \
	    ([string equal $meta(Content-Encoding) "gzip"])} {
		if {[catch {[namespace current]::feed_gzip $data} data] != 0} {
			putlog "\002RSS Error\002: Unable to decompress \"$state(url)\": $data"
			::http::cleanup $token
			return 1
		}
	}

	if {[catch {[namespace current]::xml_list_create $data} data] != 0} {
		putlog "\002RSS Error\002: Unable to parse feed properly, parser returned error. \"$state(url)\""
		::http::cleanup $token
		return 1
	}

	if {[string length $data] == 0} {
		putlog "\002RSS Error\002: Unable to parse feed properly, no data returned. \"$state(url)\""
		::http::cleanup $token
		return 1
	}

	set odata ""
	if {[catch {set odata [[namespace current]::feed_read]} error] != 0} {
		putlog "\002RSS Warning\002: $error."
	}

	if {![[namespace current]::feed_info $data]} {
		putlog "\002RSS Error\002: Invalid feed format ($state(url))!"
		::http::cleanup $token
		return 1
	}

	::http::cleanup $token

	if {[catch {[namespace current]::feed_write $data} error] != 0} {
		putlog "\002RSS Database Error\002: $error."
		return 1
	}

	if {$feed(announce-output) > 0} {
		[namespace current]::feed_output $data $odata
	}
}

proc ::rss-synd::feed_info {data {target "feed"}} {
	upvar 1 $target feed
	set length [[namespace current]::xml_get_info $data [list -1 "*"]]

	for {set i 0} {$i < $length} {incr i} {
		set type [[namespace current]::xml_get_info $data [list $i "*"] "name"]

		# tag-name: the name of the element that contains each article and its data
		# tag-list: the position in the xml structure where all 'tag-name' reside
		switch [string tolower $type] {
			rss {
				# RSS v0.9x & x2.0
				set feed(tag-list) [list 0 "channel"]
				set feed(tag-name) "item"
				break
			}
			rdf:rdf {
				# RSS v1.0
				set feed(tag-list) [list]
				set feed(tag-name) "item"
				break
			}
			feed {
				# ATOM
				set feed(tag-list) [list]
				set feed(tag-name) "entry"
				break
			}
		}
	}

	if {![info exists feed(tag-list)]} {
		return 0
	}

	set feed(tag-feed) [list 0 $type]

	return 1
}

# decompress gzip formatted data
proc ::rss-synd::feed_gzip {cdata} {
	variable packages

	if {(![info exists packages(trf)]) || \
	    ($packages(trf) != 0)} {
		error "Trf package not found."
	}

	# remove the 10 byte gzip header and 8 byte footer
	set cdata [string range $cdata 10 [expr { [string length $cdata] - 9 } ]]

	# decompress the raw data
	if {[catch {zip -mode decompress -nowrap 1 $cdata} data] != 0} {
		error $data
	}

	return $data
}

proc ::rss-synd::feed_read { } {
	upvar 1 feed feed

	if {[catch {open $feed(database) "r"} fp] != 0} {
		error $fp
	}

	set data [read -nonewline $fp]

	close $fp

	return $data
}

proc ::rss-synd::feed_write {data} {
	upvar 1 feed feed

	if {[catch {open $feed(database) "w+"} fp] != 0} {
		error $fp
	}

	set data [string map { "\n" "" "\r" "" } $data]

	puts -nonewline $fp $data

	close $fp
}

#
# XML Functions
##

proc ::rss-synd::xml_list_create {xml_data} {
	set xml_list [list]
	set ns_current [namespace current]

	set ptr 0
	while {[set tag_start [${ns_current}::xml_get_position $xml_data $ptr]] != ""} {
		set tag_start_first [lindex $tag_start 0]
		set tag_start_last [lindex $tag_start 1]

		set tag_string [string range $xml_data $tag_start_first $tag_start_last]

		# move the pointer to the next character after the current tag
		set last_ptr $ptr
		set ptr [expr { $tag_start_last + 2 }]

		array set tag [list]
		# match 'special' tags that dont close
		if {[regexp -nocase -- {^!(\[CDATA|--|DOCTYPE)} $tag_string]} {
			set tag_data $tag_string

			regexp -nocase -- {^!\[CDATA\[(.*?)\]\]$} $tag_string -> tag_data
			regexp -nocase -- {^!--(.*?)--$} $tag_string -> tag_data

			if {[info exists tag_data]} {
				set tag(data) [${ns_current}::xml_escape $tag_data]
			}
		} else {
			# we should only ever encounter opening tags, if we hit a closing one somethings wrong
			if {[string match {[/]*} $tag_string]} {
				putlog "\002RSS Malformed Feed\002: Tag not open: \"<$tag_string>\" ($tag_start_first => $tag_start_last)"
				continue
			}

			# split up the tag name and attributes
			regexp -- {(.[^ \/\n\r]*)(?: |\n|\r\n|\r|)(.*?)$} $tag_string -> tag_name tag_args
			set tag(name) [${ns_current}::xml_escape $tag_name]

			# split up all of the tags attributes
			set tag(attrib) [list]
			if {[string length $tag_args] > 0} {
				set values [regexp -inline -all -- {(?:\s*|)(.[^=]*)=["'](.[^"']*)["']} $tag_args]

				foreach {r_match r_tag r_value} $values {
					lappend tag(attrib) [${ns_current}::xml_escape $r_tag] [${ns_current}::xml_escape $r_value]
				}
			}

			# find the end tag of non-self-closing tags
			if {(![regexp {(\?|!|/)(\s*)$} $tag_args]) || \
			    (![string match "\?*" $tag_string])} {
				set tmp_num 1
				set tag_success 0
				set tag_end_last $ptr

				# find the correct closing tag if there are nested elements
				#  with the same name
				while {$tmp_num > 0} {
					# search for a possible closing tag
					set tag_success [regexp -indices -start $tag_end_last -- "</$tag_name>" $xml_data tag_end]

					set last_tag_end_last $tag_end_last

					set tag_end_first [lindex $tag_end 0]
					set tag_end_last [lindex $tag_end 1]

					# check to see if there are any NEW opening tags within the
					#  previous closing tag and the new closing one
					incr tmp_num [regexp -all -- "<$tag_name\(\[\\s\\t\\n\\r\]+\(\[^/>\]*\)?\)?>" [string range $xml_data $last_tag_end_last $tag_end_last]]

					incr tmp_num -1
				}

				if {$tag_success == 0} {
					putlog "\002RSS Malformed Feed\002: Tag not closed: \"<$tag_name>\""
					return
				}

				# set the pointer to after the last closing tag
				set ptr [expr { $tag_end_last + 1 }]

				# remember tag_start*'s character index doesnt include the tag start and end characters
				set xml_sub_data [string range $xml_data [expr { $tag_start_last + 2 }] [expr { $tag_end_first - 1 }]]

				# recurse the data within the currently open tag
				set result [${ns_current}::xml_list_create $xml_sub_data]

				# set the list data returned from the recursion we just performed
				if {[llength $result] > 0} {
					set tag(children) $result

				# set the current data we have because we're already at the end of a branch
				#  (ie: the recursion didnt return any data)
				} else {
					set tag(data) [${ns_current}::xml_escape $xml_sub_data]
				}
			}
		}

		# insert any plain data that appears before the current element
		if {$last_ptr != [expr { $tag_start_first - 1 }]} {
			lappend xml_list [list "data" [${ns_current}::xml_escape [string range $xml_data $last_ptr [expr { $tag_start_first - 2 }]]]]
		}

		# inset tag data
		lappend xml_list [array get tag]

		unset tag
	}

	# if there is still plain data left add it
	if {$ptr < [string length $xml_data]} {
		lappend xml_list [list "data" [${ns_current}::xml_escape [string range $xml_data $ptr end]]]
	}

	return $xml_list
}

# simple escape function
proc ::rss-synd::xml_escape {string} {
	regsub -all -- {([\{\}])} $string {\\\1} string

	return $string
}

# this function is to replace:
#  regexp -indices -start $ptr {<(!\[CDATA\[.+?\]\]|!--.+?--|!DOCTYPE.+?|.+?)>} $xml_data -> tag_start
# which doesnt work correctly with tcl's re_syntax
proc ::rss-synd::xml_get_position {xml_data ptr} {
	set tag_start [list -1 -1]

	regexp -indices -start $ptr {<(.+?)>} $xml_data -> tmp(tag)
	regexp -indices -start $ptr {<(!--.*?--)>} $xml_data -> tmp(comment)
	regexp -indices -start $ptr {<(!DOCTYPE.+?)>} $xml_data -> tmp(doctype)
	regexp -indices -start $ptr {<(!\[CDATA\[.+?\]\])>} $xml_data -> tmp(cdata)

	# 'tag' regexp should be compared last
	foreach name [lsort [array names tmp]] {
		set tmp_s [split $tmp($name)]
		if {( ([lindex $tmp_s 0] < [lindex $tag_start 0]) && \
		      ([lindex $tmp_s 0] > -1) ) || \
            ([lindex $tag_start 0] == -1)} {
			set tag_start $tmp($name)
		}
	}

	if {([lindex $tag_start 0] == -1) || \
	    ([lindex $tag_start 1] == -1)}  {
		set tag_start ""
	}

	return $tag_start
}

# recursivly flatten all data without tags or attributes
proc ::rss-synd::xml_list_flatten {xml_list {level 0}} {
	set xml_string ""

	foreach e_list $xml_list {
		if {[catch {array set e_array $e_list}] != 0} {
			return $xml_list
		}

		if {[info exists e_array(children)]} {
			append xml_string [[namespace current]::xml_list_flatten $e_array(children) [expr { $level + 1 }]]
		} elseif {[info exists e_array(data)]} {
			append xml_string $e_array(data)
		}

		unset e_array
	}

	return $xml_string
}

# returns information on a data structure when given a path.
#  paths can be specified using: [struct number] [struct name] <...>
proc ::rss-synd::xml_get_info {xml_list path {element "data"}} {
	set i 0

	foreach {t_data} $xml_list {
		array set t_array $t_data

		# if the name doesnt exist set it so we can still reference the data
		#  using the 'stuct name' *
		if {![info exists t_array(name)]} {
			set t_array(name) ""
		}

		if {[string match -nocase [lindex $path 1] $t_array(name)]} {

			if {$i == [lindex $path 0]} {
				set result ""

				if {([llength $path] == 2) && \
				    ([info exists t_array($element)])} {
					set result $t_array($element)
				} elseif {[info exists t_array(children)]} {
					# shift the first path reference of the front of the path and recurse
					set result [[namespace current]::xml_get_info $t_array(children) [lreplace $path 0 1] $element]
				}

				return $result
			}

			incr i
		}

		unset t_array
	}

	if {[lindex $path 0] == -1} {
		return $i
	}
}

# converts 'args' into a list in the same order
proc ::rss-synd::xml_join_tags {args} {
	set list [list]

	foreach tag $args {
		foreach item $tag {
			if {[string length $item] > 0} {
				lappend list $item
			}
		}
	}

	return $list
}

#
# Output Feed Functions
##

proc ::rss-synd::feed_output {data {odata ""}} {
	upvar 1 feed feed
	set msgs [list]

	set path [[namespace current]::xml_join_tags $feed(tag-feed) $feed(tag-list) -1 $feed(tag-name)]
	set count [[namespace current]::xml_get_info $data $path]

	for {set i 0} {($i < $count) && ($i < $feed(announce-output))} {incr i} {
		set tmpp [[namespace current]::xml_join_tags $feed(tag-feed) $feed(tag-list) $i $feed(tag-name)]
		set tmpd [[namespace current]::xml_get_info $data $tmpp "children"]

		if {[[namespace current]::feed_compare $odata $tmpd]} {
			break
		}

		set tmp_msg [[namespace current]::cookie_parse $data $i]
		if {(![info exists feed(output-order)]) || \
		    ($feed(output-order) == 0)} {
			set msgs [linsert $msgs 0 $tmp_msg]
		} else {
			lappend msgs $tmp_msg
		}
	}

	set nick [expr {[info exists feed(nick)] ? $feed(nick) : ""}]

	[namespace current]::feed_msg $feed(type) $msgs $feed(channels) $nick
}

proc ::rss-synd::feed_msg {type msgs targets {nick ""}} {
	# check if our target is a nick
	if {(($nick != "") && \
	     ($targets == "")) || \
	    ([regexp -- {[23]} $type])} {
		set targets $nick
	}

	foreach msg $msgs {
		foreach chan $targets {
			if {([catch {botonchan $chan}] == 0) || \
			    ([regexp -- {^[#&]} $chan] == 0)} {
				foreach line [split $msg "\n"] {
					if {($type == 1) || ($type == 3)} {
						putserv "NOTICE $chan :$line"
					} else {
						putserv "PRIVMSG $chan :$line"
					}
				}
			}
		}
	}
}

proc ::rss-synd::feed_compare {odata data} {
	if {$odata == ""} {
		return 0
	}

	upvar 1 feed feed
	array set ofeed [list]
	[namespace current]::feed_info $odata "ofeed"

	if {[array size ofeed] == 0} {
		putlog "\002RSS Error\002: Invalid feed format ($feed(database))!"
		return 0
	}

	if {[string equal -nocase [lindex $feed(tag-feed) 1] "feed"]} {
		set cmp_items [list {0 "id"} "children" "" 3 {0 "link"} "attrib" "href" 2 {0 "title"} "children" "" 1]
	} else {
		set cmp_items [list {0 "guid"} "children" "" 3 {0 "link"} "children" "" 2 {0 "title"} "children" "" 1]
	}

	set path [[namespace current]::xml_join_tags $ofeed(tag-feed) $ofeed(tag-list) -1 $ofeed(tag-name)]
	set count [[namespace current]::xml_get_info $odata $path]

	for {set i 0} {$i < $count} {incr i} {
		# extract the current article from the database
		set tmpp [[namespace current]::xml_join_tags $ofeed(tag-feed) $ofeed(tag-list) $i $ofeed(tag-name)]
		set tmpd [[namespace current]::xml_get_info $odata $tmpp "children"]

		set w 0; # weight value
		set m 0; # item tag matches
		foreach {cmp_path cmp_element cmp_attrib cmp_weight} $cmp_items {
			# try and extract the tag info from the current article
			set oresult [[namespace current]::xml_get_info $tmpd $cmp_path $cmp_element]
			if {$cmp_element == "attrib"} {
				array set tmp $oresult
				catch {set oresult $tmp($cmp_attrib)}
				unset tmp
			}

			# if the tag doesnt exist in the article ignore it
			if {$oresult == ""} { continue }

			incr m

			# extract the tag info from the current article
			set result [[namespace current]::xml_get_info $data $cmp_path $cmp_element]
			if {$cmp_element == "attrib"} {
				array set tmp $result
				catch {set result $tmp($cmp_attrib)}
				unset tmp
			}

			if {[string equal -nocase $oresult $result]} {
				set w [expr { $w + $cmp_weight }]
			}
		}

		# value of 100 or more means its a match
		if {($m > 0) && \
		    ([expr { round(double($w) / double($m) * 100) }] >= 100)} {
			return 1
		}
	}

	return 0
}

#
# Cookie Parsing Functions
##

proc ::rss-synd::cookie_parse {data current} {
	upvar 1 feed feed
	set output $feed(output)

	set eval 0
	if {([info exists feed(evaluate-tcl)]) && ($feed(evaluate-tcl) == 1)} { set eval 1 }
	set variable_index 0

	set matches [regexp -inline -nocase -all -- {@@(.*?)@@} $output]
	foreach {match tmpc} $matches {
		set tmpc [split $tmpc "!"]
		set index 0
		set cookie [list]
		incr variable_index
		foreach piece $tmpc {
			set tmpp [regexp -nocase -inline -all -- {^(.*?)\((.*?)\)|(.*?)$} $piece]

			if {[lindex $tmpp 3] == ""} {
				lappend cookie [lindex $tmpp 2] [lindex $tmpp 1]
			} else {
				lappend cookie 0 [lindex $tmpp 3]
			}
		}

		# replace tag-item's index with the current article
		if {[string equal -nocase $feed(tag-name) [lindex $cookie 1]]} {
			set cookie [[namespace current]::xml_join_tags $feed(tag-list) [lreplace $cookie $index $index $current]]
		}

		set cookie [[namespace current]::xml_join_tags $feed(tag-feed) $cookie]

		if {[set tmp [[namespace current]::cookie_replace $cookie $data]] != ""} {
			set tmp [[namespace current]::xml_list_flatten $tmp]

			regsub -all -- {([\"\$\[\]\{\}\(\)\\])} $match {\\\1} match
			set feed_data "[string map { "&" "\\\x26" } [[namespace current]::html_decode $eval $tmp]]"
			if {$eval == 1} {
				# We are going to eval this string so we can't insert untrusted
				# text. Instead create variables and insert references to those
				# variables that will be expanded in the subst call below.
				set cookie_val($variable_index) $feed_data
				regsub -- $match $output "\$cookie_val($variable_index)" output
			} else {
				regsub -- $match $output $feed_data output
			}
		}
	}

	# remove empty cookies
	if {(![info exists feed(remove-empty)]) || ($feed(remove-empty) == 1)} {
		regsub -nocase -all -- "@@.*?@@" $output "" output
	}

	# evaluate tcl code
	if {$eval == 1} {
		if {[catch {set output [subst $output]} error] != 0} {
			putlog "\002RSS Eval Error\002: $error"
		}
	}

	return $output
}

proc ::rss-synd::cookie_replace {cookie data} {
	set element "children"

	set tags [list]
	foreach {num section} $cookie {
		if {[string equal "=" [string range $section 0 0]]} {
			set attrib [string range $section 1 end]
			set element "attrib"
			break
		} else {
			lappend tags $num $section
		}
	}

	set return [[namespace current]::xml_get_info $data $tags $element]

	if {[string equal -nocase "attrib" $element]} {
		array set tmp $return

		if {[catch {set return $tmp($attrib)}] != 0} {
			return
		}
	}

	return $return
}

#
# Misc Functions
##

proc ::rss-synd::html_decode {eval data {loop 0}} {
	if {![string match *&* $data]} {return $data}
	array set chars {
			 nbsp	\x20 amp	\x26 quot	\x22 lt		\x3C
			 gt		\x3E iexcl	\xA1 cent	\xA2 pound	\xA3
			 curren	\xA4 yen	\xA5 brvbar	\xA6 brkbar	\xA6
			 sect	\xA7 uml	\xA8 die	\xA8 copy	\xA9
			 ordf	\xAA laquo	\xAB not	\xAC shy	\xAD
			 reg	\xAE hibar	\xAF macr	\xAF deg	\xB0
			 plusmn	\xB1 sup2	\xB2 sup3	\xB3 acute	\xB4
			 micro	\xB5 para	\xB6 middot	\xB7 cedil	\xB8
			 sup1	\xB9 ordm	\xBA raquo	\xBB frac14	\xBC
			 frac12	\xBD frac34	\xBE iquest	\xBF Agrave	\xC0
			 Aacute	\xC1 Acirc	\xC2 Atilde	\xC3 Auml	\xC4
			 Aring	\xC5 AElig	\xC6 Ccedil	\xC7 Egrave	\xC8
			 Eacute	\xC9 Ecirc	\xCA Euml	\xCB Igrave	\xCC
			 Iacute	\xCD Icirc	\xCE Iuml	\xCF ETH	\xD0
			 Dstrok	\xD0 Ntilde	\xD1 Ograve	\xD2 Oacute	\xD3
			 Ocirc	\xD4 Otilde	\xD5 Ouml	\xD6 times	\xD7
			 Oslash	\xD8 Ugrave	\xD9 Uacute	\xDA Ucirc	\xDB
			 Uuml	\xDC Yacute	\xDD THORN	\xDE szlig	\xDF
			 agrave	\xE0 aacute	\xE1 acirc	\xE2 atilde	\xE3
			 auml	\xE4 aring	\xE5 aelig	\xE6 ccedil	\xE7
			 egrave	\xE8 eacute	\xE9 ecirc	\xEA euml	\xEB
			 igrave	\xEC iacute	\xED icirc	\xEE iuml	\xEF
			 eth	\xF0 ntilde	\xF1 ograve	\xF2 oacute	\xF3
			 ocirc	\xF4 otilde	\xF5 ouml	\xF6 divide	\xF7
			 oslash	\xF8 ugrave	\xF9 uacute	\xFA ucirc	\xFB
			 uuml	\xFC yacute	\xFD thorn	\xFE yuml	\xFF
			 ensp	\x20 emsp	\x20 thinsp	\x20 zwnj	\x20
			 zwj	\x20 lrm	\x20 rlm	\x20 euro	\x80
			 sbquo	\x82 bdquo	\x84 hellip	\x85 dagger	\x86
			 Dagger	\x87 circ	\x88 permil	\x89 Scaron	\x8A
			 lsaquo	\x8B OElig	\x8C oelig	\x8D lsquo	\x91
			 rsquo	\x92 ldquo	\x93 rdquo	\x94 ndash	\x96
			 mdash	\x97 tilde	\x98 scaron	\x9A rsaquo	\x9B
			 Yuml	\x9F apos	\x27
			}

	regsub -all -- {<(.[^>]*)>} $data " " data

	if {$eval != 1} {
		regsub -all -- {([\$\[\]\{\}\(\)\\])} $data {\\\1} data
	} else {
		regsub -all -- {([\$\[\]\{\}\(\)\\])} $data {\\\\\\\1} data
	}

	regsub -all -- {&#(\d+);} $data {[subst -nocomm -novar [format \\\u%04x [scan \1 %d]]]} data
	regsub -all -- {&#x(\w+);} $data {[format %c [scan \1 %x]]} data
	regsub -all -- {&([0-9a-zA-Z#]*);} $data {[if {[catch {set tmp $chars(\1)} char] == 0} { set tmp }]} data
	regsub -all -- {&([0-9a-zA-Z#]*);} $data {[if {[catch {set tmp [string tolower $chars(\1)]} char] == 0} { set tmp }]} data

	regsub -nocase -all -- "\\s{2,}" $data " " data

	set data [subst $data]
	if {[incr loop] == 1} {
		set data [[namespace current]::html_decode 0 $data $loop]
	}

	return $data
}

proc ::rss-synd::is_utf8_patched {} { catch {queuesize a} err1; catch {queuesize \u0754} err2; expr {[string bytelength $err2]!=[string bytelength $err1]} }

proc ::rss-synd::check_channel {chanlist chan} {
	foreach match [split $chanlist] {
		if {[string equal -nocase $match $chan]} {
			return 1
		}
	}

	return 0
}

proc ::rss-synd::urldecode {str} {
	regsub -all -- {([\"\$\[\]\{\}\(\)\\])} $str {\\\1} str

	regsub -all -- {%([aAbBcCdDeEfF0-9][aAbBcCdDeEfF0-9]);?} $str {[format %c [scan \1 %x]]} str

	return [subst $str]
}

::rss-synd::init
