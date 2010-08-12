package require Tk
package require TclOO

namespace eval TkOO {
  variable widgetId
  set widgetId 0

  proc generateId {parent name} {
    namespace upvar ::TkOO widgetId id
    incr id
    set parentPath [$parent pathName]
    if {$parentPath == "."} {
      return "$parentPath$name$id"
    } else {
      return "$parentPath.$name$id"
    }
  }

  proc theme {name} {
    ::ttk::style theme use $name
  }

  oo::class create Widget {
    variable pathName

    method setup {cmd parent args} {
      set class [string tolower [lindex [split [my className] ":."] end]]
      set pathName [TkOO::generateId $parent $class]
      $cmd $pathName {*}$args
      bind <Destroy> { destroy }
    }

    method pathName {} {
      return $pathName
    }

    # forward methods to the proper tk command and memoize it
    method unknown {name args} {
      ::oo::define [my className] method $name args "::$name \$pathName {*}\$args"
      return [$name $pathName {*}$args]
    }

    method className {} {
      return [info object class [self object]]
    }
  }

  oo::class create Root {
    mixin Widget
    variable pathName

    constructor {} {
      set pathName "."
    }

    method pathName {} {
      return $pathName
    }
  }

  oo::class create Frame {
    mixin Widget
    variable pathName

    constructor {parent args} {
      my setup ::ttk::frame $parent {*}$args
    }
  }

  oo::class create Button {
    mixin Widget
    variable pathName

    constructor {parent args} {
      my setup ::ttk::button $parent {*}$args
    }
  }

  oo::class create Label {
    mixin Widget
    variable pathName

    constructor {parent args} {
      my setup ::ttk::label $parent {*}$args
    }

    method configure {args} { $pathName configure {*}$args }
  }

  oo::class create Canvas {
    mixin Widget
    variable pathName

    constructor {parent args} {
      my setup ::canvas $parent {*}$args
    }

    method create {args} { $pathName create {*}$args }
    method bind {args} { $pathName bind {*}$args }
    method coords {args} { $pathName coords {*}$args }
    method delete {args} { $pathName delete {*}$args }
  }
}

oo::class create Game {
  variable linedist boardsize gobansize goIn goOut goErr blackHistory \
           whiteHistory goban gtpid callbacks buffer status

  constructor {} {
    TkOO::theme clam

    set blackHistory [list]
    set whiteHistory [list]
    set gtpid 0
    set buffer ""
    set callbacks [dict create]
    set linedist 28
    set boardsize 9
    set gobansize [expr {$linedist * ($boardsize + 1)}]

    lassign [my popen4 gnugo --mode gtp --boardsize $boardsize] pid goIn goOut goErr

    chan event $goOut readable "[self object] goSaid \[chan read $goOut]"

    set root [TkOO::Root new]
    set f [TkOO::Frame new $root]
    set passb [TkOO::Button new $f -text Pass -command "[self object] clickedField PASS"]
    set scoreb [TkOO::Button new $f -text Score -command "[self object] score"]
    set undob [TkOO::Button new $f -text Undo -command "[self object] undo"]
    set status [TkOO::Label new $root]
    set goban [TkOO::Canvas new $root -height $gobansize -width $gobansize -background #eebb77]

    $passb  pack -padx 5 -pady 5 -side left
    $scoreb pack -padx 5 -pady 5 -side left
    $undob  pack -padx 5 -pady 5 -side left

    $status pack -side bottom -fill x
    $goban pack -expand true
    $f pack -expand true

    wm title . "Go considered"

    $root bind <Destroy> {
      my gocmd "exit" {} { exit }
    }

    my drawBoard
  }

  method drawBoard {} {
    set max [expr {$boardsize * $linedist}]

    for {set i 1} {$i <= $boardsize} {incr i} {
      set start [expr {$linedist * $i}]
      $goban create line $linedist $start $max $start
      $goban create line $start $linedist $start $max
    }

    for {set i 0} {$i < $boardsize} {incr i} {
      for {set j 0} {$j < $boardsize} {incr j} {
        set x1 [expr {(($linedist * $i) + ($linedist / 2)) - 2}]
        set y1 [expr {(($linedist * $j) + ($linedist / 2)) - 2}]
        set x2 [expr {($x1 + $linedist) - 2}]
        set y2 [expr {($y1 + $linedist) - 2}]

        set fieldname [my createFieldname $i $j]
        $goban create rectangle $x1 $y1 $x2 $y2 -tags $fieldname -outline {} -fill {}
        $goban bind $fieldname <1> "[self object] clickedField $fieldname"
      }
    }
  }

  method drawPiece {color position} {
    set coords [$goban coords $position]
    if {$coords == ""} { return }
    $goban create oval $coords -tags [list $position $color ${color}piece piece] -fill $color
  }

  method drawPieces {color positions} {
    foreach position $positions {
      my drawPiece $color $position
    }
  }

  method drawMarker {color history} {
    # whitemarker marker white
    set pos [lindex [my lselect $history {h} { expr {$h ne "PASS"} }] end]
    if {$pos eq ""} { return }
    lassign [$goban coords $pos] x1 y1 x2 y2
    set margin [expr {$linedist / 4}]
    set cs [list [expr {$x1 + $margin}] [expr {$y1 + $margin}] [expr {$x2 - $margin}] [expr {$y2 - $margin}]]

    set outline "black"
    if {$color eq "black"} { set outline "white" }
    $goban create rectangle $cs -tags [list marker ${color}marker $color] -outline $outline
  }

  method gocmd {str var callback} {
    set id $gtpid
    incr gtpid
    dict set callbacks $id [list $var $callback]
    puts $goIn "$id $str"
  }

  method clickedField {position} {
    my gocmd "play black $position" {played} "
      lappend blackHistory $position

      my gocmd {genmove white} {move} {
        my whiteMoved \$move
      }
    "
  }

  method whiteMoved {position} {
    if {$position eq "PASS"} {
      $status configure -text "White Passes"
    } else {
      lappend whiteHistory $position

      my draw black
      my draw white
    }
  }

  method draw {color} {
    my gocmd "list_stones $color" {listed} "
      \$goban delete ${color}piece ${color}marker
      my drawPieces $color \$listed
      my drawMarker $color $${color}History
    "
  }

  method score {} {
    $status configure -text "Calculating Score..."
    my gocmd "final_score" {result} {
      $status configure -text "Score: $result"
    }
  }

  method undo {} {
    my gocmd "undo" {res} {
      my gocmd "undo" {res} {
        foreach color {black white} {
          my lpop ${color}History 1
          my draw $color
        }
      }
    }
  }

  method goSaid {str} {
    append buffer $str

    while {[regexp -linestop {^([=?])(\d+)(.*?)\n\n} $buffer line mod id cmd]} {
      set buffer [string range $buffer [string bytelength $line] end]

      if {$mod eq "="} {
        lassign [dict get $callbacks $id] var callback
        eval "set $var \[list $cmd]; $callback"
      }
      if {$mod eq "?"} {
        $status configure -text "Error: $cmd"
      }
    }
  }

  # NOTE: there is no I
  method xToLetter {x} {
    lindex {A B C D E F G H J K L M N O P Q R S T} $x
  }

  method createFieldname {x y} {
    return [join [list [my xToLetter $x] [expr {$y + 1}]] ""]
  }

  method lselect {obj var cmd} {
    set result [list]
    foreach element $obj {
      if {[uplevel 1 "set $var $element; $cmd"]} {
        lappend result $element
      }
    }
    return $result
  }

  method lpop {var amount} {
    uplevel 1 "set $var \[lrange $$var 0 end-$amount]"
  }

  method popen4 {args} {
    foreach chan {In Out Err} {
      lassign [chan pipe] read$chan write$chan
    } 

    set pid [exec {*}$args <@ $readIn >@ $writeOut 2>@ $writeErr &]
    chan close $writeOut
    chan close $writeErr

    foreach chan [list stdout stderr $readOut $readErr $writeIn] {
      chan configure $chan -buffering none -blocking false
    }

    return [list $pid $writeIn $readOut $readErr]
  }
}

Game new
