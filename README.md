# moon_time
 Sunrise Sunset Functions

## Basis
Based on the sunriset.c code (included in root folder). This code is used as the basis for the Eiffel, but has not been compiled into a C project and executed to test the accuracy of the Eiffel code, which is still highly suspect. The included "tests" in the test target prove little beyond the fact that the code will execute. This does not prove the answers are correct at this time.

## Single Class
There is one class {SUNRISET} to match the C code file. This may or may not be an appropriate design.

## Differences
A number of data elements in the C code in the Eiffel have not been treated as "attributes", but as feature arguments or Result (return) values of an Eiffel Query (vs Command).

This is a matter of design choice. The design of the Eiffel code is to help preserve the Command-Query-Separation (CQS) principle. This is especially true as the basis for the design choice because nearly all of the C code functions have a return value (Query behavior) as well as making changes to attribute values (Command behavior). So, I decided to make all of the Eiffel features into Queries, where even incoming arguments may be passed back to the caller in a TUPLE, which is permissable in CQS.

## Testing
First—not every feature is presently tested. I created enough tests to make sure that the core code would run. Whether it computes correct answers is anyone's guess at this point.

Second—more effort will need to be applied to validate that the answers produced by the query-calls is actually correct.
