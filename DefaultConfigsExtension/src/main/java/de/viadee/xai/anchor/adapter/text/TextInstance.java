package de.viadee.xai.anchor.adapter.text;

import java.util.function.Function;

import de.viadee.xai.anchor.algorithm.DataInstance;

/**
 * An instance used to described a text piece (e.g. content)
 */
public class TextInstance implements DataInstance<String[]> {
    private static final Function<String, String[]> DEFAULT_TOKENIZER = s -> s.split(" ");

    private static final long serialVersionUID = 3964148380516458903L;
    private final String[] tokens;

    /**
     * @param content   the content
     * @param tokenizer the tokenizer used to extract all features
     */
    private TextInstance(final String content, final Function<String, String[]> tokenizer) {
        this(tokenizer.apply(content));
    }

    /**
     * Extracts the instances out of a string sentence by using a default tokenizer
     *
     * @param content the text instance
     */
    public TextInstance(final String content) {
        this(content, DEFAULT_TOKENIZER);
    }

    /**
     * Creates the instance by specifying all features of it
     *
     * @param tokens the tokens of the instance
     */
    public TextInstance(final String[] tokens) {
        this.tokens = tokens;
    }

    @Override
    public String[] getInstance() {
        return tokens;
    }

    /**
     * @return the instance as a joined string
     */
    public String getJoinedInstance() {
        return String.join(" ", tokens);
    }

    @Override
    public int getFeatureCount() {
        return tokens.length;
    }

    @Override
    public Object getValue(int featureId) {
        return tokens[featureId];
    }
}
