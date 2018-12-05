# JavaAnchorAdapters

> *Adapter* [/əˈdaptə/] noun, a device for connecting pieces of equipment that cannot be connected directly.

This is a collection of tools that serve to make the [Java implementation of the Anchors algorithm](https://github.com/viadee/javaAnchorServer) more easy to use. The algorithm (as introduced Marco Tulio Ribeiro, 2018) is model-agnostic, but the nature of the dataset needs to be considered. 

This repository includes *methodological* aspects, i.e. default approaches on how to apply the algorithm to tabular data in typical use cases with tabular data (such as [bpmn.ai](https://github.com/viadee/bpmn.ai)), images or texts as well as *technical* aspects, such as running Anchors explanations on Apache Spark.

This project is to be considered research-in-progress.

# Exemplary Use / Tutorial

One of this project's main feature consists in facilitating the usage of tabular explanations by providing default 
solutions to common scenarios in conjunction with the anchorj library.
The following use-case will exemplify its usage by creating both local and global explanations of the 
[Titanic tabular dataset](https://www.kaggle.com/c/titanic/data).

## 1. Referencing Dependencies
Using Apache Maven, the required anchorj dependencies can easily be referenced and added as follows:

    <!-- AnchorJ -->
    <dependency>
         <groupId>de.viadee</groupId>
         <artifactId>anchorj</artifactId>
         <version>1.0-SNAPSHOT</version>
    </dependency>
    
    <!-- AnchorJ Default Solutions Extension containing AnchorTabular -->
    <dependency>
        <groupId>de.viadee</groupId>
        <artifactId>DefaultConfigsExtension</artifactId>
        <version>1.0-SNAPSHOT</version>
    </dependency>
    
    <!-- AnchorJ Default Machine Learning Models -->
    <dependency>
        <groupId>de.viadee</groupId>
        <artifactId>DefaultMLMethods</artifactId>
        <version>1.0-SNAPSHOT</version>
    </dependency>

## 2. Loading and Describing the Dataset

<code>AnchorTabular</code> may henceforth be used to easily set up the Anchors algorithm to handle tabular data. 
Therefore, it possesses a builder that enables registering arbitrary columns that describe the dataset. 
A column contains a name for identification, a number of transformations and a discretization.
Whereas transformations are meant to clean data, discretization may be used to achieve better results with Anchors by 
grouping various feature values.

The Titanic dataset gets loaded and configured as follows. 

    AnchorTabular anchorTabular = new AnchorTabular.Builder()
            .setDoBalance(true)
            .addIgnoredColumn("PassengerId")
            .addTargetcolumn(IntegerColumn.fromStringInput("Survived"))
            .addColumn(IntegerColumn.fromStringInput("Pclass"))
            .addColumn(new StringColumn("Name"))
            .addColumn(new StringColumn("Sex"))
            .addColumn(IntegerColumn.fromStringInput("Age", -1, 5))
            .addColumn(IntegerColumn.fromStringInput("SibSp"))
            .addColumn(IntegerColumn.fromStringInput("Parch"))
            .addColumn(new StringColumn("Ticket"))
            .addColumn(DoubleColumn.fromStringInput("Fare", 6))
            .addIgnoredColumn(new StringColumn("Cabin"))
            .addColumn(new StringColumn("Embarked"))
            .build(ClassLoader.getSystemResourceAsStream("titanic/train.csv"), true, false);

Please note that attributes are described in greater depth in the code. Furthermore, all columns and components can 
be implemented and extended as required.

## 3. Obtaining the Model
As Anchors is thought to describe a model, its presence is implicitly assumed.
However, for this example a default random forest model is used to remove the need for requirements.

    TabularRandomForestClassifier classifier = TabularRandomForestClassifier
                    .createAndFit(50, anchorTabular.getTabularInstances());
                    
## 4. Obtaining the Explanation
Since both a classifier and perturbation function is now provided (latter through AnchorTabular), an 
<code>AnchorConstructionBuilder</code> can be obtained. It offers configuring various parameters of the algorithm and
can be received by the previously configured <code>AnchorTabular</code> as follows:

    AnchorConstructionBuilder<TabularInstance> defaultBuilder = anchorTabular
                    .createDefaultBuilder(classifier, anchorTabular.getTabularInstances()[0]);
                    
This builder instance can henceforth be used to create explanations (in this case for the first instance contained 
by the <code>anchorTabular</code> instance):

    AnchorResult<TabularInstance> anchor = defaultBuilder.build().constructAnchor();        
    
<code>anchor</code> now provides information about why the model predicted the instance the way it did. 
In order to make the explanation human readable, the <code>TabularInstanceVisualizer</code> provided by the 
<code>anchorTabular</code> can be used as follows:

    System.out.println("====Explained instance====" + System.lineSeparator() +
            anchorTabular.getVisualizer().visualizeInstance(anchor.getInstance()));
            
    System.out.println("====Result====" + System.lineSeparator() +
            anchorTabular.getVisualizer().visualizeResult(anchor));
            
## 5. Global Explanations
The main project contains various algorithms that are able to aggregate multiple single explanations. 
Thereof, <code>CoveragePick</code> is expected to work best. It can be used as follows:

    List<AnchorResult<TabularInstance>> globalExplanations = new CoveragePick<>(defaultBuilder, 10)
                    .run(anchorTabular.shuffleSplitInstances(1, 0)[0], 20);          

Similarly, its results may be visualized
    
    System.out.println(anchorTabular.getVisualizer().visualizeGlobalResults(globalExplanations));
    
## 6. Exemplary Outputs
The above stated examples produce output similar to the following samples.

    ====Explained instance====
        Pclass='3'
        Name='Gilnagh, Miss. Katherine 'Katie''
        Sex='female'
        Age='16'
        SibSp='0'
        Parch='0'
        Ticket='35851'
        Fare='7'
        Embarked='Q'
        WITH LABEL Survived='1'
    ====Result====
        IF Sex='female' {0.85,-0.58} AND 
        Embarked='Q' {0.11,-0.37} AND 
        SibSp='0' {0.04,-0} AND 
        Parch='0' {0.02,-0}
        THEN PREDICT 1
        WITH PRECISION 1.0 AND COVERAGE 0.033

    
    ===Global Result #1===
        IF Fare IN RANGE [0,8] {0.63,-0.64} AND 
        Sex='male' {0.37,-0.07} AND 
        Parch='0' {0.01,-0}
        THEN PREDICT 0
        WITH PRECISION 1.0 AND COVERAGE 0.283
    ===Global Result #2===
        IF Fare IN RANGE [53,512] {0.68,-0.8} AND 
        Sex='female' {0.29,-0.07} AND 
        Pclass='1' {0.04,-0}
        THEN PREDICT 1
        WITH PRECISION 1.0 AND COVERAGE 0.117
    ===Global Result #3===
        IF SibSp='8' {0.64,-0.99} AND 
        Pclass='3' {0.06,0} AND 
        Ticket='CA. 2343' {0.19,0} AND 
        Embarked='S' {0.09,0} AND 
        Age IN RANGE [-1,0] {0.04,0}
        THEN PREDICT 0
        WITH PRECISION 1.0 AND COVERAGE 0.007
    ===Global Result #4===
        IF Fare IN RANGE [27,52] {0.61,-0.87} AND 
        Name='Barkworth, Mr. Algernon Henry Wilson' {0.24,-0.12} AND 
        Ticket='27042' {0.16,0}
        THEN PREDICT 1
        WITH PRECISION 1.0 AND COVERAGE 0.002
    ===Global Result #5===
        IF Fare IN RANGE [15,26] {0.55,-0.81} AND 
        Parch='2' {0.19,-0.16} AND 
        SibSp='1' {0.12,-0.02} AND 
        Name='Dean, Master. Bertram Vere' {0.16,0}
        THEN PREDICT 1
        WITH PRECISION 1.0 AND COVERAGE 0.002


# Collaboration

The project is operated and further developed by the viadee Consulting AG in Münster, Westphalia. Results from theses at the WWU Münster and the FH Münster have been incorporated.
* Further theses are planned: Contact person is Dr. Frank Köhne from viadee.
    Community contributions to the project are welcome: Please open Github-Issues with suggestions (or PR), which we can then edit in the team.
*   We are looking for further partners who have interesting process data to refine our tooling as well as partners that are simply interested in a discussion about AI in the context of business process automation and explainability.
