package de.viadee.xai.anchor.adapter.classifiers;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import de.viadee.xai.anchor.adapter.util.CountVectorizer;
import de.viadee.xai.anchor.adapter.LabeledInstanceList;
import de.viadee.xai.anchor.adapter.text.TextInstance;

/**
 * Abstract classifier enabling predictions of String data by using vectorization
 */
public abstract class AbstractTextEnabledNumericClassifier implements NumericClassifier {
    private static final long serialVersionUID = -1193554418216143033L;

    private static final Pattern WORD_PATTERN = Pattern.compile("(?u)\\b\\w\\w+\\b");
    private CountVectorizer vectorizer;

    /**
     * Fits the model using {@link TextInstance}s
     *
     * @param labeledInstanceList the labeled instance list
     */
    public void fit(LabeledInstanceList<TextInstance> labeledInstanceList) {
        if (vectorizer == null) {
            vectorizer = CountVectorizer.create().withMinimalDocumentFrequency(1).build();
        }
        double[][] vectorizedData = vectorize(labeledInstanceList.getInstances().stream().map(TextInstance::getJoinedInstance).collect(Collectors.toList()), true);
        fit(vectorizedData, labeledInstanceList.getLabels().stream().mapToInt(i -> i).toArray());
    }

    /**
     * Predict the label using string data.
     *
     * @param data the data
     * @return the int
     */
    public int predict(String data) {
        double[] vectorized = vectorize(Collections.singletonList(data), false)[0];
        return predict(vectorized);
    }

    private double[][] vectorize(Collection<? extends String> documents, boolean doFit) {
        if (vectorizer == null)
            throw new IllegalArgumentException("No vectorizer initialized. First fit the data.");

        List<List<String>> vectorData = new ArrayList<>();
        for (String document : documents) {
            Matcher m = WORD_PATTERN.matcher(document);
            List<String> tokens = new ArrayList<>();
            while (m.find()) {
                tokens.add(document.substring(m.start(), m.end()).toLowerCase());
            }
            vectorData.add(tokens);
        }
        if (doFit) {
            vectorizer.fit(vectorData);
        }
        return vectorizer.transform(vectorData).toArray();
    }
}
