from selenium import webdriver
from selenium.webdriver.common.keys import Keys
import time

browser = webdriver.Firefox()

browser.get('http://localhost:9000')

assert 'opus' in browser.title

def press(text): 
   browser.find_element_by_link_text(text).click()

def fail_login(user, password):
   browser.get('http://localhost:9000/login')
   browser.find_element_by_name('login').send_keys(user)
   browser.find_element_by_name('password').send_keys(password + Keys.RETURN)
   assert user + "@opus" not in browser.title
   assert "Hello " + user + "!" not in browser.page_source
   return
   
def login(user, password):
   browser.get('http://localhost:9000/login')
   elem = browser.find_element_by_name('login')
   elem.send_keys(user)
   elem = browser.find_element_by_name('password')
   elem.send_keys(password)
   elem.send_keys(Keys.RETURN)
   assert user + "@opus" in browser.title
   assert "Hello " + user + "!" in browser.page_source
   return

def add_new_blag(id, content):
   browser.find_element_by_link_text('new').click()
   browser.find_element_by_name("id").send_keys(id)
   browser.find_element_by_id("ed").send_keys(content)
   browser.find_element_by_name("op").click()
   assert "ok-note" in browser.page_source

def see(str):
   assert str in browser.page_source

def see_ok():
   assert "<div class=\"ok-note\">" in browser.page_source

def see_fail():
   assert "<div class=\"fail" in browser.page_source
   
def nosee(str):
   assert str not in browser.page_source
   
def seetag(str):
   assert ("\">" + str + "\n") in browser.page_source
   
def noseetag(str):
   assert ("\">" + str + "\n") not in browser.page_source
   
def search(str):
   browser.find_element_by_name('q').send_keys(str + Keys.RETURN)

def goto(str):
   browser.get('http://localhost:9000/n/' + str)

def goto_raw(str):
   browser.get('http://localhost:9000/' + str)
   
def logout():
   goto_raw("logout")

def add_user(name, hash, session):
   browser.find_element_by_link_text('new').click()
   browser.find_element_by_name("id").send_keys(name)
   browser.find_element_by_id("ed").send_keys("# home of " + name + "\n#hash:" + hash + " #session:" + session)
   browser.find_element_by_xpath("//select[@name='type']/option[text()='user']").click()
   browser.find_element_by_name("op").click()
   assert "ok-note" in browser.page_source

def add_kal(name, content):
   browser.find_element_by_link_text('new').click()
   browser.find_element_by_name("id").send_keys(name)
   browser.find_element_by_id("ed").send_keys(content)
   browser.find_element_by_xpath("//select[@name='type']/option[text()='kal']").click()
   browser.find_element_by_name("op").click()
   see_ok()
   
def fail_add_kal(name, content):
   browser.find_element_by_link_text('new').click()
   browser.find_element_by_name("id").send_keys(name)
   browser.find_element_by_id("ed").send_keys(content)
   browser.find_element_by_xpath("//select[@name='type']/option[text()='kal']").click()
   browser.find_element_by_name("op").click()
   see_fail()
 
# login after a few failures
fail_login("test", "bad")
fail_login("bad", "pass")
fail_login("xxx", "xxx")
login("test", "pass")

# empty search
search("sisalto")
see("nothing suitable")

# posting content 
add_new_blag("testipage", "# testisivu\nsisalto")
add_new_blag("testi", "# toka testisivu\nsisalto myos")
search("sisalto")
see("2 matches")

add_new_blag("foobarbaz", "# ABC\n#foo #bar #baz")
add_new_blag("barbaz", "# BC\n#bar #baz")
add_new_blag("foobaz", "# AC\n#foo #baz")
add_new_blag("foobar", "# AB\n#foo #bar")
add_new_blag("foo", "# A\n#foo")
add_new_blag("bar", "# B\n#bar")
add_new_blag("baz", "# C\n#baz")

press("idx")
seetag("ABC")
seetag("AB")
seetag("BC")
seetag("AC")
seetag("A")
seetag("B")
seetag("C")
nosee("Selected")

press("foo")
seetag("ABC")
seetag("AB")
noseetag("BC")
seetag("AC")
seetag("A")
noseetag("B")
noseetag("C")
see("Selected")

press("bar")
seetag("ABC")
seetag("AB")
noseetag("BC")
noseetag("AC")
noseetag("A")
noseetag("B")
noseetag("C")

press("baz")
seetag("ABC")
noseetag("AB")
noseetag("BC")
noseetag("AC")
noseetag("A")
noseetag("B")
noseetag("C")

press("foo")
seetag("ABC")
noseetag("AB")
seetag("BC")
noseetag("AC")
noseetag("A")
noseetag("B")
noseetag("C")

press("bar")
seetag("ABC")
noseetag("AB")
seetag("BC")
seetag("AC")
noseetag("A")
noseetag("B")
seetag("C")

press("baz")
seetag("ABC")
seetag("AB")
seetag("BC")
seetag("AC")
seetag("A")
seetag("B")
seetag("C")
nosee("Selected")

add_user("user1", "df6bbacc20a3ce53c6c4f8c787e05feb263e7f7ddbff6c76aa5e7462d890ed5a", "session1")
see("ok-note")

add_user("user2", "a02c5c3f386e38a2547c19e4b0d646bedb5b1ece4b0983a25bdbc8203b9e8eb5", "session2")
see("ok-note")

# check that pages of created users are not visible
goto("user1")
nosee("home of")
goto("user1.txt")
nosee("home of")
goto("user1.owl")
nosee("home of")
goto("/")
# check that content of the created users are not searchable
search("home of")
nosee("home of")
# check that user pages cannot be deleted
goto("/n/delete/user1")
see_fail()
# check that the content cannot be opened for editing
goto("/n/edit/user1")
see_fail()

add_kal("kalen", "1.1.2048\n - do stuff")
goto("kalen")
see("do stuff")

fail_add_kal("kalenx", "0.1.2016\n - magic")
fail_add_kal("kalenx", "1.0.2016\n - magic")
fail_add_kal("kalenx", "32.1.2016\n - magic")
fail_add_kal("kalenx", "1.13.2016\n - magic")
goto("kalenx")
nosee("magic") # 404 is just text, fix later to be an opus page

goto("/") # no links visible after 404 atm
add_kal("kalenx", "every day: foo\n every day: bar")
goto("kalenx")
see("every day")
see("Monday, week")

# webroot and directory traversal tests
goto_raw("f/hello.txt")
see("Hello, world!")
goto_raw("f/../webroot/hello.txt")
see_fail()
goto_raw("f/../../../../../../etc/passwd")
see_fail()
goto_raw("../../../../../../../etc/passwd")
see_fail()
goto_raw("../../../../../../../etc/passwd")
see_fail()
goto_raw("f/./hello.txt")
see("Hello, world!")
goto_raw("f/./../.../...../......./........./etc/passwd")
see_fail()

logout()
see("bye bye")

# add_new_blag("success", "# SUCCCESS\nEverything seems to be in order")
time.sleep(1)

# see("slartibartfast") # break to see state for new tests

browser.quit()

