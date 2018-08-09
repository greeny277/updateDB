Explanation of module and my approach
=====

Author
---

Name: Christian Bay
Mail: christian.bay@posteo.de

Tools
---

- Building tool [stack](https://docs.haskellstack.org/en/stable/README/) (Version 1.7.1)
- Documentation tool [haddock](https://www.haskell.org/haddock/) (Version 2.18.1)

Preliminary
----

This little program is a result of an exercise a company sent to me for a job application.

The task that needs to be solved is quite simple: You have a PostgreSQL table
of persons consisting of four rows: First name, last name, date of birth and telephone number.
This first three one uniquely identify each person. The table looks like the following:

```sql
CREATE TABLE person (
	fname character varying,
	lname character varying,
	dob date,
	phone character(10),
	UNIQUE (fname, lname, dob)
);
```

Now, a XML file is handed to you, which has updated or new entries for persons. Either
a new entry needs to be inserted or an existing one needs to be updated. The only updated row
is the telephone number. The update should be applied as fast as possible to the table.


Approaches
----

While working on this exercise I had three different approaches.

At first I thought that i want to parse the XML file and then create an own table.
Afterwards I merge them by using an Upsert query that inserts.
For this I used Haskell's
[Cursor library](http://hackage.haskell.org/package/xml-conduit-1.8.0/docs/Text-XML-Cursor.html).
But this library reads in the content of the whole XML first and continues processing only
afterwards.

In order prevent loading huge amount of megabytes into RAM I decided to use the standard library for
streaming [Conduit](https://github.com/snoyberg/conduit#readme) and process the parsed tokens
sequentially.

With this streaming library in hand I started parsing the XML file and inserting the entries into a
new table. Sadly this took for about 30 minutes. I thought that it can be optimized on two fronts.
On the one hand the new created database is only of temporary use. After the Upsert process it will
always be dropped. So writing its results down to the hard-drive is quite unnecessary. On the other
hand, I want to prevent inserting 1.5 Mio entries from the XML file in separate queries.

After searching a lot I found finally a solution for both of my problems
[this](https://www.safaribooksonline.com/library/view/practical-postgresql/9781449309770/ch04s03.html#example_ascii_copy_file).
The built-in COPY command is able to read in data from files like CSV in a very fast way. And it is
possible to create temporary databases.

With this in hand I used my streaming module to parse the XML and save it as CSV file. Afterwards a
temporary database is created and filled with data from the CSV file by using the COPY command.
At last I use an Insert/Update query to merge both.

The whole process takes about 4-5 minutes on a T430 Lenovo Thinkpad.

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


Discussion
---

I think regarding the speed its a good solution. Yet, I do not like that the updated row
is explicitly named in the Update/Insert query. In case the person scheme is more complex both,
the queries and the datatype representing which represents a single person needs to be adjusted. For
the first problem a suitable query exists, I guess. For the datatype representation I do not see
an easy solution. One might to consider to do without a Person datatype and just send the parsed
contents of the XML tags directly to a CSV file. But the tags still need to be named, so what's the
point?
