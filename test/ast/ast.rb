#!/usr/bin/ruby
#
# test

require 'rexml/document'
require 'Pathname'
require 'open3'
require 'tmpdir'

$XML
$UNIT
$CASE

def fail
    puts "\033[1;31mFAILED\033[0m"
    puts "#{$XML}:unit #{$UNIT}:case #{$CASE}"
    exit 1
end

def ok
   puts "\033[32mOK\033[0m"
end

def cc_code(code)
    path = Dir.mktmpdir("mcc") + "/1.c"
    f = File.open(path, "w")
    f.puts code
    f.close
    stdout, stderr, status = Open3.capture3("./mcc " + path)
    if status != 0 then fail end
    stderr
end

def to_xml(output)
    puts output
end

def diff(output, expect)
    
end

def run_case(e)
    code = e.elements.find { |k| k.name == "code" }
    expect = e.elements.find { |k| k.name == "expect" }
    output = cc_code code.text
    diff(to_xml(output), expect)
end

def run_unit(unit)
    $UNIT = unit.attributes["name"]
    if $UNIT == nil
       $UNIT = File.basename($XML, File.extname($XML))
    end
    print "Testing " + $UNIT + " ..."
    cases = unit.elements.select { |k| k.name == "case" }
    cases.each do |e|
        $CASE = unit.index e
        run_case e
    end
end

def parse_xml(path)
    (REXML::Document.new File.open path).root
end

def main
    dir = Pathname.new(__FILE__).dirname
    xmls = Dir.glob dir + "*.xml"
    xmls.each do |xml|
        path = Pathname.pwd + xml
        $XML = xml
        run_unit parse_xml path
        ok
    end
end

main