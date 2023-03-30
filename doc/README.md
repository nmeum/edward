# API Documentation

This generates the API documentation for the edward library interface.

## Dependencies

In order to generate the documentation the following packages need to be installed:

* [GNU make][gnu make]
* [Discount][discount web]
* [scmdoc][scmdoc github]

## Usage

After installing the aforementioned dependency, run the following command:

    $ make -C doc

This will generate several HTML files which provide an API documentation for R7RS libraries provided by edward.
An index page which links to these files is also generated.
Open `doc/index.html` in your favorite web browser to view this index page.

[gnu make]: https://www.gnu.org/software/make
[discount web]: http://www.pell.portland.or.us/~orc/Code/discount/
[scmdoc github]: https://github.com/nmeum/scmdoc
