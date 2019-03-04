[![License](https://img.shields.io/badge/License-BSD%203--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)
[![Build Status](https://travis-ci.org/viadee/javaAnchorAdapters.svg?branch=master)](https://travis-ci.org/viadee/javaAnchorAdapters)
[![Sonarcloud Coverage](https://sonarcloud.io/api/project_badges/measure?project=de.viadee.xai.anchor:anchorjAdapters&metric=coverage)](https://sonarcloud.io/dashboard?id=de.viadee.xai.anchor%3AanchorjAdapters) 

# JavaAnchorAdapters

> *Adapter* [/əˈdaptə/] noun, a device for connecting pieces of equipment that cannot be connected directly.

This is a collection of tools that serve to make the [Java implementation of the Anchors algorithm](https://github.com/viadee/javaAnchorExplainer) more easy to use. The algorithm (as introduced Marco Tulio Ribeiro, 2018) is model-agnostic, but the nature of the dataset needs to be considered. 

This repository includes *methodological* aspects, i.e. default approaches on how to apply the algorithm to tabular data in typical use cases with tabular data (such as [bpmn.ai](https://github.com/viadee/bpmn.ai)), images or texts as well as *technical* aspects, such as running Anchors explanations on Apache Spark.

This project is to be considered research-in-progress.

## Why Java?
Java has been chosen as the platform's foundation, since it provides multiple advantages: 
it integrates well into a large ecosystem and can be used in conjunction with advanced technologies like H2O and 
Apache Spark. 

This implementation furthermore serves as a library based on which more approaches can be developed. 
Among others, adapters, interfaces and API's are in development to offer the opportunity of platform-independent access.

It is thus expected to reach a high dissemination among ML projects.

# Exemplary Use / Tutorial

Examples of using the Anchors implementation and its various adapters are provided within the [XAI Examples](https://github.com/viadee/xai_examples) repository. 
Please refer to this project for tutorials and easy-to-run applications.

# Collaboration

The project is operated and further developed by the viadee Consulting AG in Münster, Westphalia. Results from theses at the WWU Münster and the FH Münster have been incorporated.
* Further theses are planned: Contact person is Dr. Frank Köhne from viadee.
    Community contributions to the project are welcome: Please open Github-Issues with suggestions (or PR), which we can then edit in the team.
*   We are looking for further partners who have interesting process data to refine our tooling as well as partners that are simply interested in a discussion about AI in the context of business process automation and explainability.
