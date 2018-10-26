# Codebook Generator

Forked from [MartinLBarron/CodebookGenerator](https://github.com/MartinLBarron/CodebookGenerator), this script generates a codebook from a given data frame. An R Markdown document and a knitted HTML file are created. I have made many modifications to the code (which was originally part of the [dataMaid package](https://github.com/ekstroem/dataMaid)) to better fit the needs of my collaborator.

This code takes advantage of special attributes added to variables in a data set: 

- labels (from [Hmisc](https://github.com/harrelfe/Hmisc))
- a logical flag indicating whether the variable is part of the REDCap database or if it was derived
- how the variable was derived (if applicable)
- any variable-specific comments