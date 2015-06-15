What is IvanOS?
====================

IvanOS is an [open source](http://www.apache.org/licenses/LICENSE-2.0)
[distributed operating
system](http://en.wikipedia.org/wiki/Distributed_operating_system).

IvanOS is targeting data owners and applications developers, specifically
data intensive
[applications](http://en.wikipedia.org/wiki/Application_software) that
exhibit [massive
concurrency](http://en.wikipedia.org/wiki/Concurrency_%28computer_science%29)
and distributed scale over some period of time.  IvanOS is focused on
helping minimize software development and operational costs for these data
intensive applications.  We assume that these applications demand
[continuous operation](http://en.wikipedia.org/wiki/Continuous_operation)
on lots of [distributed
commodity](http://en.wikipedia.org/wiki/Commodity_computing)
infrastructure.

IvanOS allows data owners to maximize time to value (i.e. data insights
and resulting actions) while minimizing total cost of ownership and
eliminating vendor lock-in.

Um, OK, let's put it this way:

> *IvanOS combines the OS, networking (SDN), hardware drivers, APIS, language
bingings, GUI hooks, Statistical Libraries, Machine leraning libraries,
Platform wide metadata, Platform wide scheduler, concurrency and hardware
scaling into a single vertical bundle, and wraps it in a language (Erlang)
that can be used by a software developer.*


OK, Lets do this!
---------------------

* Show me [some code](code.html)
* Show me some hardware!  
* Show me some strange architecture diagrams! 
* Show me someone who cares

Hasn't this been done before?
---------------------
IvanOS is not focused on [deployment, scaling, and consolidation of legacy
applications or traditional three-tiered web applications](notivanos.html)

What are your initial goals?
---------------------

IvanOS is initially attempting to service [streaming analytic applications](http://www.datanami.com/2014/07/22/streaming-analytics-ready-prime-time-forrester-says/), which are massively concurrent, data intensive, and distributed

What is the relationship between IvanOS and Erlang?
---------------------

As an operating system designed for massively concurrent applications,
IvanOS uses the [Erlang](http://www.erlang.org/) programming language as its system language. 

Erlang is to IvanOS as the [C programming](http://en.wikipedia.org/wiki/C_%28programming_language%29) language is to [UNIX](http://en.wikipedia.org/wiki/Unix) and Unix-like
operating systems, most notably
[Linux](http://en.wikipedia.org/wiki/Linux).  This choice of Erlang as the
IvanOS system language in no way implies that all IvanOS application
developers will need to know Erlang.  We fully expect to support many
other languages in time.  Python, Go, and Elixir are likely first on the
list.

As a distributed operating system, IvanOS takes the Erlang based [Open
Telecom Platform
(OTP)](http://www.erlang.org/doc/design_principles/users_guide.html) as starting point, reference, cautionary tale, and
inspiration.  In many respects, IvanOS is a logical, albeit ambitious,
modernization and expansion of the the OTP.

Do I need to know Erlang to contribute?
-------------------------------------

Yes. 

Before you groan and gnash your teeth, you might consider a few
things.  Just like we all had to consider learining [object oriented
programming](http://en.wikipedia.org/wiki/Object-oriented_programming) in the 1990s, it is likely that most programmers in the 2020s
will need to learn some programming language which fundamentally supports
concurrency and the [functional programming
style](http://en.wikipedia.org/wiki/Functional_programming).

> *"Of the languages that use a message-passing concurrency model, Erlang is
probably the most widely used in industry at present."*
    [Wikipedia](https://en.wikipedia.org/wiki/Concurrent_computing)

It's actually [easy and fun to learn
Erlang](http://learnyousomeerlang.com/).

Erlang has an awesome open source [culture and
community](http://www.erlang-factory.com/).


How is IvanOS different from other distributed operating systems?
-----------------


What are some example IvanOS applications?
-----------------


Where is the system documentation?
-----------------


Why is IvanOS an operating system?
-----------------



Who is Ivan?
====================

Ivan Pulleyn is the son, brother, friend, and colleague of some of the key
contributors to and supporters of IvanOS.  Ivan is a key contributor to
IvanOS.  Ivan passed away in 2012.  For those who knew Ivan, this effort
is a tribute to his spirit and his ideas.  For everyone involved in
IvanOS, his name is a reminder that this project is a community effort and
belongs to no one.

