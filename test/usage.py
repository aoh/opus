from selenium import webdriver
from selenium.webdriver.common.keys import Keys

browser = webdriver.Firefox()

browser.get('http://localhost:9000')

assert 'opus' in browser.title

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
   
def search(str):
   browser.find_element_by_name('q').send_keys(str + Keys.RETURN)

fail_login("test", "bad")
fail_login("bad", "pass")
fail_login("xxx", "xxx")
login("test", "pass")

search("sisalto")
see("nothing suitable")

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

browser.quit()

