## Config File Specification
Config file is in JSON format, which could be easily generated using most programming language.

Individual entries like question, title, text information are all modeled as `Element`. `Element` is represented by a JSON dictionary.

A sample survey could be found [here](./sample.json), you can upload it in the [website](http://ec2-184-73-150-230.compute-1.amazonaws.com/static/index.html) to build the survey from it.

##### Radiogroup
```javascript
{ "type":"radiogroup", "title":str, "choices": [str, str, ...], "colCount": Int optional}
```
The specification is mostly self-explanatory. `colCount` determined the number of options displayed in each row. 

##### Text input
```javascript
{"type":"title", "title":str}
```

##### Title
```javascript
{ "type":"title", "title":str}
```

##### Plain Text
`Plain Text` is simply a JSON string. It is used to provide additional information.

##### List
```javascript
[Element1, Element2, Element3, ...]
```
A `List` contains one or many elements and represented by a JSON array. `List` can be arbitrarily nested since itself is an element.

