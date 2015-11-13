# Gaia Files Specifications

Unlike Nyx which had a very strict json schema for the Nyx objects, Gaia, doesn't have nyx objects, and instead is driven by Gaia files. A text file is a Gaia file if it has the following attributes
	- It is named 'gaia'
	- the first line is just made of the four letters gaia followed by end of line "\n".

Then the rest of the file consists in *directives*, one per line. A directive is a *directive name*, for instance *tag* followed by a semi colon followed by the *body* of the directive. The exact grammar of a directive is ([a-zA-Z0-9_]+):(.*)\n


```
tag: Use the force, Luke
```



A line whose first visible character is # is a comment to be ignored. 

# Directives

- tag: Indicates that the folder containing the gaia file is to be returned in a search compatible with the body of the tag. The body of the tag is a one line string of unspecified length.


