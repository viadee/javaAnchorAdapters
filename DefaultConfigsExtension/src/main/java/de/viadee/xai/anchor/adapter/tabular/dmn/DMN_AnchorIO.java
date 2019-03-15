package de.viadee.xai.anchor.adapter.tabular.dmn;

/**
 * class for Anchor In- and OutputFeatures
 */
public class DMN_AnchorIO {

    private String name;
    private String type;

    /**
     * creates new In or Output
     * @param name name of feature, will be label to
     * @param type type of feature
     */
    public DMN_AnchorIO(String name, String type) {
        this.name = name;
        this.type = type;
    }

    /**
     * used for Set
     * @return int for Set check for equals
     */
    public int hashCode(){
        int hashcode = 0;
        hashcode = name.hashCode();
        hashcode += type.hashCode();
        return hashcode;
    }

    /**
     * used for Set
     * @param obj used for Set
     * @return bool if already in Set
     */
    public boolean equals(Object obj){
        if (obj instanceof DMN_AnchorIO) {
            DMN_AnchorIO pp = (DMN_AnchorIO) obj;
            return (pp.name.equals(this.name) && pp.type == this.type);
        } else {
            return false;
        }
    }

    /**
     * get Name
     * @return String of Featurename
     */
    public String getName() {
        return name;
    }

    /**
     * sets Featurename
     * @param name name of Feature
     */
    public void setName(String name) {
        this.name = name;
    }

    /**
     * get Type of Feature
     * @return type of Feature
     */
    public String getType() {
        return type;
    }

    /**
     * sets type of feature
     * @param type Type as String
     */
    public void setType(String type) {
        this.type = type;
    }

}
