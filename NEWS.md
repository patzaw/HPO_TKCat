<!----------------------------------------------------------------------------->
<!----------------------------------------------------------------------------->
## Version 0.8 -- Release notes

### Implementation changes
   
   - Dealing with "#" header in the *phenotype_annotation.tab* source file

<!----------------------------------------------------------------------------->
<!----------------------------------------------------------------------------->
## Version 0.7 -- Release notes

### Implementation changes
   
   - json data model


<!----------------------------------------------------------------------------->
<!----------------------------------------------------------------------------->
## Version 0.6 -- Release notes

### Data model changes
   
   - Add collections


<!----------------------------------------------------------------------------->
<!----------------------------------------------------------------------------->
## Version 0.5 -- Release notes

### Data model changes

   - new "HPO_synonyms" table
   - new "level" field in the "HPO_hp" table

### Implementation changes

	- Using tibbles

<!----------------------------------------------------------------------------->
<!----------------------------------------------------------------------------->
## Version 0.4 -- Release notes

### Data model changes

   - new "HPO_parents" table
   - new "HPO_descendants" table
   - remove "parent" field from the "HPO_hp" table
   - new "level" field in the "HPO_hp" table

### Implementation changes

	- Using the "here" package for locating files

<!----------------------------------------------------------------------------->
<!----------------------------------------------------------------------------->
## Version 0.3 -- Release notes

### Implementation changes

	- Tables are exported with " quotes for delimiting fields and escaped when needed by doubling them
