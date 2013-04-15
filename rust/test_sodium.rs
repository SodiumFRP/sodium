extern mod std;
extern mod sodium;
use sodium::event::*;
use sodium::transaction::*;

/*
    event_sink<int> ev;
    std::shared_ptr<string> out(new string);
    ev.send('?');
    function<void()> unlisten;
    {
        transaction<> trans;
        ev.send('h');
        unlisten = ev.listen([out] (int ch) {
            *out = *out + (char)ch;
        });
        ev.send('e');
    };
    {
        transaction<> trans;
        ev.send('l');
        ev.send('l');
        ev.send('o');
    }
    unlisten();
    ev.send('!');
    CPPUNIT_ASSERT_EQUAL(string("hello"), *out);
*/

fn event1(def_part : Partition<DefPart>) {
    let out = @mut ~[];
    let (e, send_e) = Event::new(def_part);
    let unlisten = do sync(&def_part) || {
        send_e('h');
        let unlisten = e.listen(|x| { out.push(x); });
        send_e('e');
        unlisten
    };
    send_e('l');
    send_e('l');
    send_e('o');
    unlisten();
    //io::println((copy *out).len().to_str());
    assert!(*out == ~['h','e','l','l','o']);
}

fn main() {
    let def_part : Partition<DefPart> = Partition::new(def_part_key);
    event1(def_part);
    io::println("pass");
}
