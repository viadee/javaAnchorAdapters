package de.viadee.xai.anchor.adapter.tabular.dmn;

import de.viadee.xai.anchor.adapter.tabular.TabularInstance;
import de.viadee.xai.anchor.adapter.tabular.column.DoubleColumn;
import de.viadee.xai.anchor.adapter.tabular.column.GenericColumn;
import de.viadee.xai.anchor.adapter.tabular.column.IntegerColumn;
import de.viadee.xai.anchor.adapter.tabular.column.StringColumn;
import de.viadee.xai.anchor.adapter.tabular.discretizer.DiscretizerRelation;
import de.viadee.xai.anchor.algorithm.AnchorResult;
import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import java.io.OutputStream;
import java.math.RoundingMode;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.util.*;

/**
 * DMN_Explainer translates the gloabExplanation into a DMN-Standard xml file
 * works for tabular data with single Outputs TODO: more than 1 output
 */
public class DMN_Explainer {

    /**
     * extracts information about Inputs, Outputs and rules out of the globalExplanations
     * and calls the buildDMN method that generates the DMN-xml file
     *
     * @param globalExplanations List of Rules
     * @param outputStream       stream
     */
    public static void createDMNglobalExplanation(List<AnchorResult<TabularInstance>> globalExplanations,
                                                  OutputStream outputStream) {

        // inputList is a Set of all Inputs that are present in globalExplanations
        Set<DMN_AnchorIO> inputList = new HashSet<>();

        // output is the output-Variable that is predicted in globalExplanations
        DMN_AnchorIO output = null;

        // ruleList is a List of all rules that are present in globalExplanations
        List<DMN_AnchorRule> ruleList = new ArrayList<>();


        // iterates over globalExplanations until all Inputs are found
        for (AnchorResult<TabularInstance> ar : globalExplanations) {

            TabularInstance instance = ar.getInstance();
            DecimalFormat df = new DecimalFormat("#.##");
            df.setRoundingMode(RoundingMode.CEILING);
            df.setDecimalFormatSymbols(DecimalFormatSymbols.getInstance(Locale.US));
            Iterator var5 = ar.getOrderedFeatures().iterator();

            while (var5.hasNext()) {
                Integer featureNr = (Integer) var5.next();
                GenericColumn feature = instance.getFeatures()[featureNr];

                String featureType = getType(feature);

                inputList.add(new DMN_AnchorIO(feature.getName(), featureType));
            }
        }

        // get Output of globalExplanations
        TabularInstance instance2 = globalExplanations.get(0).getInstance();

        GenericColumn outputFeature = instance2.getTargetFeature();
        String outputType = getType(outputFeature);

        output = new DMN_AnchorIO(outputFeature.getName(), outputType);

        // creates Array for ordered definition of inputs to allow systematic creation of rules below
        String[] ruleFeatures = new String[inputList.size()];

        int i = 0;
        for (DMN_AnchorIO ai : inputList) {
            ruleFeatures[i] = ai.getName();
            i++;
        }

        for (AnchorResult<TabularInstance> ar : globalExplanations) {
            String[] ruleValues = new String[inputList.size()];

            for (int a = 0; a < ruleValues.length; a++) {
                ruleValues[a] = "";
            }

            TabularInstance instance = ar.getInstance();
            DecimalFormat df = new DecimalFormat("#.##");
            df.setRoundingMode(RoundingMode.CEILING);
            df.setDecimalFormatSymbols(DecimalFormatSymbols.getInstance(Locale.US));
            Iterator var5 = ar.getOrderedFeatures().iterator();

            while (var5.hasNext()) {
                Integer featureNr = (Integer) var5.next();
                GenericColumn feature = instance.getFeatures()[featureNr];

                int index = Arrays.asList(ruleFeatures).indexOf(feature.getName());

                String featureValue;

                DiscretizerRelation relation = feature.getDiscretizer().unApply(instance.getValue(feature));
                switch (relation.getFeatureType()) {
                    case METRIC:
                        featureValue = "[" + relation.getConditionMin() + ".." + relation.getConditionMax() + "]";
                        break;
                    case CATEGORICAL:
                        featureValue = relation.getCategoricalValue().toString();
                        break;
                    default:
                        featureValue = "";
                }

                ruleValues[index] = featureValue;
            }

            ruleList.add(new DMN_AnchorRule("(⌐■_■)", ruleValues, ar.getInstance().getDiscretizedLabel().toString(), ar.getPrecision(), ar.getCoverage()));
        }

        buildDMN(inputList, output, ruleList, outputStream);
    }

    /**
     * gets the Type of a GenericColumn and returns the name of the Type in String, for use in DMN-xml file
     *
     * @param feature
     * @return String with lowercase name of Type, for the DMN-xml file
     */
    private static String getType(GenericColumn feature) {

        if (feature instanceof IntegerColumn) {
            return "integer";
        } else if (feature instanceof StringColumn) {
            return "string";
        } else if (feature instanceof DoubleColumn) {
            return "double";
        } else {
            return "string"; //TODO
        }
    }

    /**
     * adds a new Element to the doc
     *
     * @param document the document where the element is added into
     * @param label    name of the element
     * @param element  parentElement of the to-be-created Element
     * @return
     */
    private static Element addElement(Document document, String label, Element element) {
        Element elem = document.createElement(label);
        element.appendChild(elem);
        return elem;
    }

    /**
     * adds a new Element with Textnode Childelement
     *
     * @param document  the document where the element is added into
     * @param label     name of the element
     * @param element   parentElement of the to-be-created Element
     * @param innerText content of the childElement
     */
    private static void addElementWithText(Document document, String label, Element element, String innerText) {
        Element elem = document.createElement(label);
        elem.appendChild(document.createTextNode(innerText));
        element.appendChild(elem);
    }

    /**
     * adds Attributes to an element
     *
     * @param document  the document where the element is added into
     * @param keyValues "key" = value
     * @param element   parentElement of the to-be-created Attributes
     */
    private static void addAttributes(Document document, String[][] keyValues, Element element) {
        for (String[] values : keyValues) {
            Attr attr = document.createAttribute(values[0]);
            attr.setValue(values[1]);
            element.setAttributeNode(attr);
        }
    }

    /**
     * creates the DMN-xml file
     *
     * @param inputList    List of Inputs with name and type in String
     * @param outputAnchor Output with name and type in String
     * @param ruleList     List of rules, with all corrected InputValues for all inputs
     * @param outputStream stream
     */
    public static void buildDMN(Set<DMN_AnchorIO> inputList, DMN_AnchorIO outputAnchor, List<DMN_AnchorRule> ruleList,
                                OutputStream outputStream) {

        try {
            DocumentBuilderFactory dbFactory =
                    DocumentBuilderFactory.newInstance();
            DocumentBuilder dBuilder = dbFactory.newDocumentBuilder();
            Document doc = dBuilder.newDocument();


            // Basic structure of any DMN-Decision Table
            Element definitions = doc.createElement("definitions");
            doc.appendChild(definitions);

            addAttributes(doc, new String[][]{
                    new String[]{"xmlns", "http://www.omg.org/spec/DMN/20151101/dmn.xsd"},
                    new String[]{"xmlns:camunda", "http://camunda.org/schema/1.0/dmn"},
                    new String[]{"name", "definitions"},
                    new String[]{"id", "definitions_" + randomIdString()}
            }, definitions);

            Element decision = addElement(doc, "decision", definitions);

            //TODO: name aus Anchors ziehen?
            addAttributes(doc, new String[][]{
                    new String[]{"id", "decision_" + randomIdString()},
                    new String[]{"name", "Anchor zu DMN"}
            }, decision);

            Element decisionTable = addElement(doc, "decisionTable", decision);

            addAttributes(doc, new String[][]{
                    new String[]{"hitPolicy", "FIRST"},
                    new String[]{"id", "decisionTable_" + randomIdString()}
            }, decisionTable);


            // Elements and Attributes which depend on the individual Machine Learning Model
            for (DMN_AnchorIO a : inputList) {
                Element input = addElement(doc, "input", decisionTable);


                addAttributes(doc, new String[][]{
                        new String[]{"id", "InputClause_" + randomIdString()},
                        new String[]{"label", a.getName().toLowerCase()},
                        new String[]{"camunda:inputVariable", a.getName().toLowerCase()},
                }, input);

                Element inputExpression = addElement(doc, "inputExpression", input);

                addAttributes(doc, new String[][]{
                        new String[]{"id", "LiteralExpresson_" + randomIdString()},
                        new String[]{"typeRef", a.getType()},
                        new String[]{"expressionLanguage", "FEEL"},
                }, inputExpression);

                addElementWithText(doc, "text", inputExpression, a.getName());
            }

            Element output = addElement(doc, "output", decisionTable);

            addAttributes(doc, new String[][]{
                    new String[]{"id", "OutputClause_" + randomIdString()},
                    new String[]{"typeRef", outputAnchor.getType()},
                    new String[]{"label", outputAnchor.getName().toLowerCase()},
                    new String[]{"name", outputAnchor.getName()},
            }, output);

            Element outputPrecision = addElement(doc, "output", decisionTable);

            addAttributes(doc, new String[][]{
                    new String[]{"id", "OutputClause_" + randomIdString()},
                    new String[]{"typeRef", "double"},
                    new String[]{"label", "precision"},
                    new String[]{"name", "Precision"},
            }, outputPrecision);

            Element outputCoverage = addElement(doc, "output", decisionTable);

            addAttributes(doc, new String[][]{
                    new String[]{"id", "OutputClause_" + randomIdString()},
                    new String[]{"typeRef", "double"},
                    new String[]{"label", "coverage"},
                    new String[]{"name", "Coverage"},
            }, outputCoverage);


            for (DMN_AnchorRule ar : ruleList) {
                Element rule = addElement(doc, "rule", decisionTable);

                addAttributes(doc, new String[][]{
                        new String[]{"id", "DecisionRule_" + randomIdString()},
                }, rule);

                addElementWithText(doc, "description", rule, ar.getDescription());

                for (String s : ar.getInputEntryList()) {
                    Element inputEntry = addElement(doc, "inputEntry", rule);

                    addAttributes(doc, new String[][]{
                            new String[]{"id", "UnaryTests_" + randomIdString()},
                    }, inputEntry);

                    addElementWithText(doc, "text", inputEntry, s);
                }

                Element outputEntry = addElement(doc, "outputEntry", rule);

                addAttributes(doc, new String[][]{
                        new String[]{"id", "LiteralExpression_" + randomIdString()},
                }, outputEntry);

                addElementWithText(doc, "text", outputEntry, ar.getOutputEntry());

                Element outputEntryPrecision = addElement(doc, "outputEntry", rule);

                addAttributes(doc, new String[][]{
                        new String[]{"id", "LiteralExpression_" + randomIdString()},
                }, outputEntryPrecision);

                addElementWithText(doc, "text", outputEntryPrecision, String.valueOf(ar.getPrecision()));

                Element outputEntryCoverage = addElement(doc, "outputEntry", rule);

                addAttributes(doc, new String[][]{
                        new String[]{"id", "LiteralExpression_" + randomIdString()},
                }, outputEntryCoverage);

                addElementWithText(doc, "text", outputEntryCoverage, String.valueOf(ar.getCoverage()));
            }


            // write the content into xml file and outputStream
            TransformerFactory transformerFactory = TransformerFactory.newInstance();
            Transformer transformer = transformerFactory.newTransformer();
            DOMSource source = new DOMSource(doc);

            StreamResult result = new StreamResult(outputStream);

            transformer.transform(source, result);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * creates a String for the id-Attributes in DMN-xml file
     *
     * @return alphanumeric String with 7 characters
     */
    private static String randomIdString() {
        return UUID.randomUUID().toString().toLowerCase().replace("-", "").substring(1, 8);
    }
}
