Explanation of module and my approach
=====

Tools
---

- Building tool [stack](https://docs.haskellstack.org/en/stable/README/) (Version 1.7.1)
- Documentation tool [haddock](https://www.haskell.org/haddock/) (Version 2.18.1)

Preliminary
----

In the conceptual formulation of the exercise the following was written:

"There is a PostgreSQL table of persons (person), uniquely identified
by their first name (fname), last name (lname) and date of birth
(dob). [...]"

But, the given SQL file creates a table without marking this triple as UNIQUE. Therefore
I changed this in the SQL file as following:

CREATE TABLE person (
	fname character varying,
	lname character varying,
	dob date,
	phone character(10),
	UNIQUE (fname, lname, dob)
);

Approaches
----

While working on this exercise I had three different approaches.

At first I thought that i want to parse the XML file and then create an own table.
Afterwards I merge them by using an Upsert query that inserts.
For this I used Haskells
[Cursor library](http://hackage.haskell.org/package/xml-conduit-1.8.0/docs/Text-XML-Cursor.html).
But this library reads in the content of the whole XML first and continues processing only
afterwards.

In order prevent loading huge amount of megabytes into RAM I decided to use the standard library for
streaming [Conduit](https://github.com/snoyberg/conduit#readme) and process the parsed tokens
sequentially.

With this streaming library in hand I started parsing the XML file and inserting the entries into a
new table. Sadly this took for about 30 minutes. I thought that it can be optimized on two fronts.
On the one hand the new created database is only of temporary use. After the Upsert process it will
always be dropped. So writing its results done to the harddisk is quite unnecessary. On the other
hand, I want to prevent inserting 1.5 Mio entries from the XML file in separate queries.

After searching a lot I found finally a solution for both of my problems
[this](https://www.safaribooksonline.com/library/view/practical-postgresql/9781449309770/ch04s03.html#example_ascii_copy_file).
The built-in COPY command is able to read in data from files like CSV in a very fast way. And it is
possible to create temporary databases.

With this in hand I used my streaming module to parse the XML and save it as CSV file. Afterwards a
temporary database is created and filled with data from the CSV file by using the COPY command.
At last I use an Insert/Update query to merge both.


Usage
---

To build the tool use:

``` bash
stack build
```

To exec the tool use:

``` bash
stack exec updateDB-exe
```

The program will ask for several input data. Like the name of the database, username, password, path
to source files.
It is recommended to save the XML and CSV file in data.
