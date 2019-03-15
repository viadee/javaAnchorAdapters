package de.viadee.xai.anchor.adapter.tabular.dmn;

/**
 * class for Anchorrules
 */
public class DMN_AnchorRule {

    private String description;
    private String[] inputEntryList;
    private String outputEntry;
    private double precision;
    private double coverage;

    /**
     * create a new Rule
     * @param description allows for comments in the DMN decision table
     * @param inputEntryList Values of every Input in the inputlist, "" if not relevant for rule
     * @param outputEntry Predicted output
     * @param precision double value of rules precision
     * @param coverage double value of rules coverage
     */
    public DMN_AnchorRule(String description, String[] inputEntryList, String outputEntry, double precision, double coverage) {
        this.description = description;
        this.inputEntryList = inputEntryList;
        this.outputEntry = outputEntry;
        this.precision = precision;
        this.coverage = coverage;
    }

    /**
     * getter description
     * @return get description
     */
    public String getDescription() {
        return description;
    }

    /**
     * setter description
     * @param description setter
     */
    public void setDescription(String description) {
        this.description = description;
    }

    /**
     * getter inputEntrylist
     * @return get Entrylist
     */
    public String[] getInputEntryList() {
        return inputEntryList;
    }

    /**
     * setter inputEntryList
     * @param inputEntryList setter
     */
    public void setInputEntryList(String[] inputEntryList) {
        this.inputEntryList = inputEntryList;
    }

    /**
     * getter outputEntry
     * @return get Outputentry
     */
    public String getOutputEntry() {
        return outputEntry;
    }

    /**
     * setter outputEntry
     * @param outputEntry setter
     */
    public void setOutputEntry(String outputEntry) {
        this.outputEntry = outputEntry;
    }

    /**
     * getter Precision
     * @return get Precision
     */
    public double getPrecision() {
        return precision;
    }

    /**
     * setter Precision
     * @param precision setter
     */
    public void setPrecision(double precision) {
        this.precision = precision;
    }

    /**
     * getter coverage
     * @return get coverage
     */
    public double getCoverage() {
        return coverage;
    }

    /**
     * setter coverage
     * @param coverage setter
     */
    public void setCoverage(double coverage) {
        this.coverage = coverage;
    }
}
