# Aion Points

Aion points will be the data points that are put in xcache. 
They are JSON objects that are serialized before being committed to disk, 
and represent the file systems trees that are scanned and against which queries can the ran. 
They are called after `aion` (Pascal's backup/snapshot program). 

We have essentially two objects types, which represent files and folders. 

```
{
	"aion-type" : "file"
	"version"   : 1
	"name"      : String
	"size"      : Integer
	"hash"      : sha1-hash
}
```

```
{
	"aion-type" : "directory"
	"version"   : 1
	"name"      : String
	"contents"  : CAS-KEY(s)
}
```

We use the expression "CAS-KEY" for the key returned by the Content Addressable Store.