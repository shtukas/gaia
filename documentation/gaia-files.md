# Gaia Files Specifications

Unlike Nyx which had a very strict json schema for the Nyx objects, Gaia, doesn't have nyx objects, and instead is driven by Gaia files.
A text file is a Gaia file if it has the following attributes

	- It is named `gaia`
	- the first line is just made of the four letters `gaia` followed by a new line `\n`.

Then the rest of the file consists in *directives*, one per line, *empty lines* or *comments*. 

- A directive is a *directive name*, for instance *tag* followed by a *semicolon* followed by the *body* of the directive. 
The exact grammar of a directive is `([a-zA-Z0-9_]+):\s*(.*)\n`. 
Directives must be terminated with a new line. E.g.

	```
	tag: Use the force, Luke
	tag: UTF-8 body is allowed: ü è £ ∞ ∫
	
	```

- A line whose first character is `#` is a comment to be ignored. 
The exact grammar of a comment is `#.*\n`. 
Comments must be terminated with a new line. E.g.

	```
	# this is a comment
	# this is another comment
	
	# this is not a comment and would invalidate the configuration file
	```

- Empty lines are ignored.

# Directives

- `tag`: indicates that the folder containing the Gaia file is to be returned in a search compatible with the body of the tag. 
The body of the tag is a one line string of unspecified length.


