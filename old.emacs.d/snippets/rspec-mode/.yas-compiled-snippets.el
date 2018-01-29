;;; Compiled snippets and support files for `snippets'
;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("wia" "with(${1:args})\n$0" "with args" nil
                        ("rspec-mode")
                        nil "/Users/Andy/.emacs.d/snippets/rspec-mode/wia" nil nil)
                       ("tw" "twice" "twice" nil
                        ("rspec-mode")
                        nil "/Users/Andy/.emacs.d/snippets/rspec-mode/tw" nil nil)
                       ("stub" "${1:target}.stub!(:${2:message})$0" "$target.stub!(:$message)" nil
                        ("rspec-mode")
                        nil "/Users/Andy/.emacs.d/snippets/rspec-mode/stub" nil nil)
                       ("sto" "Story '${1:title}', %{\n  As a ${2:role}\n  I want ${3:feature}\n  So that ${4:value}\n} do\nend" "Story" nil
                        ("rspec-mode")
                        nil "/Users/Andy/.emacs.d/snippets/rspec-mode/sto" nil nil)
                       ("sht" "lambda { ${1: } }.should throw_symbol(:${2:symbol})\n$0" "should throw" nil
                        ("rspec-mode")
                        nil "/Users/Andy/.emacs.d/snippets/rspec-mode/sht" nil nil)
                       ("shs" "${1:target}.should satisfy { |obj| ${2: } }\n$0" "should satisfy" nil
                        ("rspec-mode")
                        nil "/Users/Andy/.emacs.d/snippets/rspec-mode/shs" nil nil)
                       ("shrt.respond" "${1:target}.should respond_to(:${2:sym})\n$0" "should respond_to" nil
                        ("rspec-mode")
                        nil "/Users/Andy/.emacs.d/snippets/rspec-mode/shrt.respond" nil nil)
                       ("shrt.render" "response.should render_template(:${1:template})\n$0" "should render_template" nil
                        ("rspec-mode")
                        nil "/Users/Andy/.emacs.d/snippets/rspec-mode/shrt.render" nil nil)
                       ("shrt.redirect" "response.should redirect_to(${1:url})\n$0" "should redirect_to" nil
                        ("rspec-mode")
                        nil "/Users/Andy/.emacs.d/snippets/rspec-mode/shrt.redirect" nil nil)
                       ("shrt" "response.should render_template(\"${0:template}\")" "response.should render_template(\"$template\")" nil
                        ("rspec-mode")
                        nil "/Users/Andy/.emacs.d/snippets/rspec-mode/shrt" nil nil)
                       ("shre" "lambda { ${1: } }.should raise_error(${2:error})\n$0" "should raise_error" nil
                        ("rspec-mode")
                        nil "/Users/Andy/.emacs.d/snippets/rspec-mode/shre" nil nil)
                       ("shr" "${1:mock}.should_receive(:${2:message})$0" "should_receive" nil
                        ("rspec-mode")
                        nil "/Users/Andy/.emacs.d/snippets/rspec-mode/shr" nil nil)
                       ("shp" "${1:target}.should ${2:be_}${3:predicate} $0" "should predicate" nil
                        ("rspec-mode")
                        nil "/Users/Andy/.emacs.d/snippets/rspec-mode/shp" nil nil)
                       ("shnt" "lambda { ${1: } }.should_not throw_symbol(:${2:symbol})\n$0" "should_not throw" nil
                        ("rspec-mode")
                        nil "/Users/Andy/.emacs.d/snippets/rspec-mode/shnt" nil nil)
                       ("shns" "${1:target}.should_not satisfy { |obj| ${2: } }\n$0" "should_not satisfy" nil
                        ("rspec-mode")
                        nil "/Users/Andy/.emacs.d/snippets/rspec-mode/shns" nil nil)
                       ("shnrt" "${1:target}.should_not respond_to(:${2:sym})\n$0" "should_not respond_to" nil
                        ("rspec-mode")
                        nil "/Users/Andy/.emacs.d/snippets/rspec-mode/shnrt" nil nil)
                       ("shnre" "lambda { ${1: } }.should_not raise_error(${2:error})\n$0" "should_not raise_error" nil
                        ("rspec-mode")
                        nil "/Users/Andy/.emacs.d/snippets/rspec-mode/shnre" nil nil)
                       ("shnr" "${1:mock}.should_not_receive(:${2:message})$0" "should_not_receive" nil
                        ("rspec-mode")
                        nil "/Users/Andy/.emacs.d/snippets/rspec-mode/shnr" nil nil)
                       ("shnp" "${1:target}.should_not ${2:be_}${3:predicate} $0" "should_not predicate" nil
                        ("rspec-mode")
                        nil "/Users/Andy/.emacs.d/snippets/rspec-mode/shnp" nil nil)
                       ("shnm.match" "${1:target}.should_not match(/${2:regex}/)\n$0" "should_not match" nil
                        ("rspec-mode")
                        nil "/Users/Andy/.emacs.d/snippets/rspec-mode/shnm.match" nil nil)
                       ("shne.equal" "${1:target}.should_not equal(${2:value})\n$0" "should_not equal" nil
                        ("rspec-mode")
                        nil "/Users/Andy/.emacs.d/snippets/rspec-mode/shne.equal" nil nil)
                       ("shne.eql" "${1:target}.should_not eql(${2:value})\n$0" "should_not eql" nil
                        ("rspec-mode")
                        nil "/Users/Andy/.emacs.d/snippets/rspec-mode/shne.eql" nil nil)
                       ("shnbs" "response.should_not be_success\n$0" "should_not be_success" nil
                        ("rspec-mode")
                        nil "/Users/Andy/.emacs.d/snippets/rspec-mode/shnbs" nil nil)
                       ("shnbr" "response.should_not be_redirect\n$0" "should_not be_redirect" nil
                        ("rspec-mode")
                        nil "/Users/Andy/.emacs.d/snippets/rspec-mode/shnbr" nil nil)
                       ("shnbko" "${1:target}.should_not be_a_kind_of(${2:klass})\n$0" "should_not be_kind_of" nil
                        ("rspec-mode")
                        nil "/Users/Andy/.emacs.d/snippets/rspec-mode/shnbko" nil nil)
                       ("shnbio" "${1:target}.should_not be_instance_of(${2:klass})\n$0" "should_not be_instance_of" nil
                        ("rspec-mode")
                        nil "/Users/Andy/.emacs.d/snippets/rspec-mode/shnbio" nil nil)
                       ("shnbc" "${1:target}.should_not be_close(${2:result}, ${3:tolerance})\n$0" "should_not be_close" nil
                        ("rspec-mode")
                        nil "/Users/Andy/.emacs.d/snippets/rspec-mode/shnbc" nil nil)
                       ("shnb" "${1:target}.should_not be(${2:result})\n$0" "should_not be" nil
                        ("rspec-mode")
                        nil "/Users/Andy/.emacs.d/snippets/rspec-mode/shnb" nil nil)
                       ("shn=" "${1:target}.should_not == ${2:value}\n$0" "should_not ==" nil
                        ("rspec-mode")
                        nil "/Users/Andy/.emacs.d/snippets/rspec-mode/shn=" nil nil)
                       ("shm.match" "${1:target}.should match(/${2:regex}/)\n$0" "should match" nil
                        ("rspec-mode")
                        nil "/Users/Andy/.emacs.d/snippets/rspec-mode/shm.match" nil nil)
                       ("shhr" "${1:target}.should have(${2:x}).records\n$0" "should have_records" nil
                        ("rspec-mode")
                        nil "/Users/Andy/.emacs.d/snippets/rspec-mode/shhr" nil nil)
                       ("shham" "${1:target}.should have_at_most(${2:num}).${3:things}\n$0" "should have_at_most" nil
                        ("rspec-mode")
                        nil "/Users/Andy/.emacs.d/snippets/rspec-mode/shham" nil nil)
                       ("shhal" "${1:target}.should have_at_least(${2:num}).${3:things}\n$0" "should have_at_least" nil
                        ("rspec-mode")
                        nil "/Users/Andy/.emacs.d/snippets/rspec-mode/shhal" nil nil)
                       ("shh" "${1:target}.should have(${2:num}).${3:things}\n$0" "should have" nil
                        ("rspec-mode")
                        nil "/Users/Andy/.emacs.d/snippets/rspec-mode/shh" nil nil)
                       ("she.equal" "${1:target}.should equal(${2:value})\n$0" "should equal" nil
                        ("rspec-mode")
                        nil "/Users/Andy/.emacs.d/snippets/rspec-mode/she.equal" nil nil)
                       ("she.eql" "${1:target}.should eql(${2:value})\n$0" "should eql" nil
                        ("rspec-mode")
                        nil "/Users/Andy/.emacs.d/snippets/rspec-mode/she.eql" nil nil)
                       ("shbs" "response.should be_success\n$0" "should be_success" nil
                        ("rspec-mode")
                        nil "/Users/Andy/.emacs.d/snippets/rspec-mode/shbs" nil nil)
                       ("shbr" "response.should be_redirect\n$0" "should be_redirect" nil
                        ("rspec-mode")
                        nil "/Users/Andy/.emacs.d/snippets/rspec-mode/shbr" nil nil)
                       ("shbko" "${1:target}.should be_a_kind_of(${2:klass})\n$0" "should be_kind_of" nil
                        ("rspec-mode")
                        nil "/Users/Andy/.emacs.d/snippets/rspec-mode/shbko" nil nil)
                       ("shbio" "${1:target}.should be_instance_of(${2:klass})\n$0" "should be_instance_of" nil
                        ("rspec-mode")
                        nil "/Users/Andy/.emacs.d/snippets/rspec-mode/shbio" nil nil)
                       ("shbc" "${1:target}.should be_close(${2:result}, ${3:tolerance})\n$0" "should be_close" nil
                        ("rspec-mode")
                        nil "/Users/Andy/.emacs.d/snippets/rspec-mode/shbc" nil nil)
                       ("shb" "${1:target}.should be(${2:result})\n$0" "should be" nil
                        ("rspec-mode")
                        nil "/Users/Andy/.emacs.d/snippets/rspec-mode/shb" nil nil)
                       ("sh=" "${1:target}.should == ${2:value}\n$0" "should ==" nil
                        ("rspec-mode")
                        nil "/Users/Andy/.emacs.d/snippets/rspec-mode/sh=" nil nil)
                       ("set" "setup do\n  $1\nend" "setup do ... end" nil
                        ("rspec-mode")
                        nil "/Users/Andy/.emacs.d/snippets/rspec-mode/set" nil nil)
                       ("sce" "Scenario '${1:title}' do\n  Given '${2:given}'\n  When '${3:when}'\n  Then '${4:then}'\nend\n$0" "Scenario" nil
                        ("rspec-mode")
                        nil "/Users/Andy/.emacs.d/snippets/rspec-mode/sce" nil nil)
                       ("resh" "require File.dirname(__FILE__) + '/../spec_helper'" "Require spec_helper" nil
                        ("rspec-mode")
                        nil "/Users/Andy/.emacs.d/snippets/rspec-mode/resh" nil nil)
                       ("on" "once" "once" nil
                        ("rspec-mode")
                        nil "/Users/Andy/.emacs.d/snippets/rspec-mode/on" nil nil)
                       ("mocw.rr" "Spec::Runner.configure do |config|\n  config.mock_with :rr\nend" "mock_with rr" nil
                        ("rspec-mode")
                        nil "/Users/Andy/.emacs.d/snippets/rspec-mode/mocw.rr" nil nil)
                       ("mocw.mocha" "Spec::Runner.configure do |config|\n  config.mock_with :mocha\nend" "mock_with mocha" nil
                        ("rspec-mode")
                        nil "/Users/Andy/.emacs.d/snippets/rspec-mode/mocw.mocha" nil nil)
                       ("mocw.flexmock" "Spec::Runner.configure do |config|\n  config.mock_with :flexmock\nend" "mock_with flexmock" nil
                        ("rspec-mode")
                        nil "/Users/Andy/.emacs.d/snippets/rspec-mode/mocw.flexmock" nil nil)
                       ("mat" "class ${1:ReverseTo}\n  def initialize($3)\n    @$3 = $3\n  end\n\n  def matches?(actual)\n    @actual = actual\n    # Satisfy expectation here. Return false or raise an error if it's not met.\n    $0@actual.reverse.should == @$3\n    true\n  end\n\n  def failure_message\n    \"expected #{@actual.inspect} to $2 #{@$3.inspect}, but it didn't\"\n  end\n\n  def negative_failure_message\n    \"expected #{@actual.inspect} not to $2 #{@$3.inspect}, but it did\"\n  end\nend\n\ndef ${2:reverse_to}(${3:expected})\n  $1.new($3)\nend" "custom matcher" nil
                        ("rspec-mode")
                        nil "/Users/Andy/.emacs.d/snippets/rspec-mode/mat" nil nil)
                       ("its" "its(:${1}) { should $0 }" "its" nil
                        ("rspec-mode")
                        nil "/Users/Andy/.emacs.d/snippets/rspec-mode/its" nil nil)
                       ("it" "it \"should ${1:do something}\" do\n  $0\nend" "it \"should do something\" do ... end" nil
                        ("rspec-mode")
                        nil "/Users/Andy/.emacs.d/snippets/rspec-mode/it" nil nil)
                       ("fmar" "${1:var} = flexmock(:model, ${2:YourModel})\n$0" "flexmock mock object, ActiveRecord" nil
                        ("rspec-mode")
                        nil "/Users/Andy/.emacs.d/snippets/rspec-mode/fmar" nil nil)
                       ("fm" "${1:var} = flexmock('${2:mock_name}')\n$0" "flexmock mock object w/name" nil
                        ("rspec-mode")
                        nil "/Users/Andy/.emacs.d/snippets/rspec-mode/fm" nil nil)
                       ("ex" "exactly(${1:n}).times" "exactly" nil
                        ("rspec-mode")
                        nil "/Users/Andy/.emacs.d/snippets/rspec-mode/ex" nil nil)
                       ("dests" "describe ${1:Type}, '${2:description}' do\n\n  it 'should ${3:description}' do\n    $0\n  end\n\nend" "describe (type, string)" nil
                        ("rspec-mode")
                        nil "/Users/Andy/.emacs.d/snippets/rspec-mode/dests" nil nil)
                       ("dest" "describe ${1:Type} do\n\n  it 'should ${2:description}' do\n    $0\n  end\n\nend" "describe (type)" nil
                        ("rspec-mode")
                        nil "/Users/Andy/.emacs.d/snippets/rspec-mode/dest" nil nil)
                       ("desrc.put" "require File.direname(__FILE__) + '/.../spec_helper'\n\ndescribe ${1:controller}, 'PUT ${3:/some/path}${4: with some parameters}' do\n\n  $0\n\nend" "describe (RESTful Controller): PUT" nil
                        ("rspec-mode")
                        nil "/Users/Andy/.emacs.d/snippets/rspec-mode/desrc.put" nil nil)
                       ("desrc.post" "require File.direname(__FILE__) + '/.../spec_helper'\n\ndescribe ${1:controller}, 'POST ${3:/some/path}${4: with some parameters}' do\n\n  $0\n\nend" "describe (RESTful Controller): POST" nil
                        ("rspec-mode")
                        nil "/Users/Andy/.emacs.d/snippets/rspec-mode/desrc.post" nil nil)
                       ("desrc.get" "require File.direname(__FILE__) + '/.../spec_helper'\n\ndescribe ${1:controller}, 'GET ${3:/some/path}${4: with some parameters}' do\n\n  $0\n\nend" "describe (RESTful Controller): GET" nil
                        ("rspec-mode")
                        nil "/Users/Andy/.emacs.d/snippets/rspec-mode/desrc.get" nil nil)
                       ("desrc.delete" "require File.direname(__FILE__) + '/.../spec_helper'\n\ndescribe ${1:controller}, 'DELETE ${3:/some/path}${4: with some parameters}' do\n\n  $0\n\nend" "describe (RESTful Controller): DELETE" nil
                        ("rspec-mode")
                        nil "/Users/Andy/.emacs.d/snippets/rspec-mode/desrc.delete" nil nil)
                       ("desc" "require File.dirname(__FILE__) + '/../spec_helper'\n\ndescribe ${1:controller} do\n\n  $0\n\nend" "describe (Controller)" nil
                        ("rspec-mode")
                        nil "/Users/Andy/.emacs.d/snippets/rspec-mode/desc" nil nil)
                       ("des" "describe '${1:description}' do\n\n  it 'should ${2:description}' do\n    $0\n  end\n\nend" "describe (String)" nil
                        ("rspec-mode")
                        nil "/Users/Andy/.emacs.d/snippets/rspec-mode/des" nil nil)
                       ("conn" "controller_name :${1:controller}" "controller_name" nil
                        ("rspec-mode")
                        nil "/Users/Andy/.emacs.d/snippets/rspec-mode/conn" nil nil)
                       ("bfe" "before(:each) do\n  $0\nend" "before(:each) do ... end" nil
                        ("rspec-mode")
                        nil "/Users/Andy/.emacs.d/snippets/rspec-mode/bfe" nil nil)
                       ("befm" "before(:each) do\n  @${1:model} = ${1:$(replace-regexp-in-string \"_\" \"\" (upcase-initials text))}.new$0\nend\n" "before (rspec)" nil
                        ("rspec-mode")
                        nil "/Users/Andy/.emacs.d/snippets/rspec-mode/befm" nil nil)
                       ("bef" "before(${1::each}) do\n  $0\nend" "before" nil
                        ("rspec-mode")
                        nil "/Users/Andy/.emacs.d/snippets/rspec-mode/bef" nil nil)
                       ("atm" "at_most(${1:n}).times" "at_most" nil
                        ("rspec-mode")
                        nil "/Users/Andy/.emacs.d/snippets/rspec-mode/atm" nil nil)
                       ("atl" "at_least(${1:n}).times" "at_least" nil
                        ("rspec-mode")
                        nil "/Users/Andy/.emacs.d/snippets/rspec-mode/atl" nil nil)
                       ("any" "any? { |${e}| $0 }" "any? { |...| ... }" nil
                        ("rspec-mode")
                        nil "/Users/Andy/.emacs.d/snippets/rspec-mode/any" nil nil)
                       ("ant" "and_throw(${1:sym})" "and_throw" nil
                        ("rspec-mode")
                        nil "/Users/Andy/.emacs.d/snippets/rspec-mode/ant" nil nil)
                       ("anrb" "and_return { $1 }" "and_return with block" nil
                        ("rspec-mode")
                        nil "/Users/Andy/.emacs.d/snippets/rspec-mode/anrb" nil nil)
                       ("anra" "and_raise(${1:RuntimeError})$0" "and_raise($error)" nil
                        ("rspec-mode")
                        nil "/Users/Andy/.emacs.d/snippets/rspec-mode/anra" nil nil)
                       ("anr" "and_return(${1:value})$0" "and_return($value)" nil
                        ("rspec-mode")
                        nil "/Users/Andy/.emacs.d/snippets/rspec-mode/anr" nil nil)
                       ("annot" "any_number_of_times" "any_number_of_times" nil
                        ("rspec-mode")
                        nil "/Users/Andy/.emacs.d/snippets/rspec-mode/annot" nil nil)
                       ("aft" "after(${1::each}) do\n  $0\nend" "after" nil
                        ("rspec-mode")
                        nil "/Users/Andy/.emacs.d/snippets/rspec-mode/aft" nil nil)))


;;; Do not edit! File generated at Tue May 17 21:10:01 2016
