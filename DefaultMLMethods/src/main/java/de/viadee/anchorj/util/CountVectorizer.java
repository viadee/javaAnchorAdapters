package de.viadee.anchorj.util;

import com.google.common.collect.*;
import com.google.common.collect.Multiset.Entry;
import smile.data.SparseDataset;

import java.io.Serializable;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Partially adapted from <a href="https://gist.github.com/alexeygrigorev/6238341f79bd81b79879607226120ae4">
 * https://gist.github.com/alexeygrigorev/6238341f79bd81b79879607226120ae4</a>
 */
public final class CountVectorizer implements Serializable {
    private static final long serialVersionUID = -8758157039821033639L;

    /**
     * The type Count vectorizer builder.
     */
    public static class CountVectorizerBuilder {
        private int minDf = 5;
        private boolean applyIdf = true;
        private boolean sublinearTf = false;
        private boolean normalize = true;

        /**
         * With minimal document frequency count vectorizer builder.
         *
         * @param minDf the min df
         * @return the count vectorizer builder
         */
        public CountVectorizerBuilder withMinimalDocumentFrequency(int minDf) {
            this.minDf = minDf;
            return this;
        }

        /**
         * With idf transformation count vectorizer builder.
         *
         * @return the count vectorizer builder
         */
        public CountVectorizerBuilder withIdfTransformation() {
            this.applyIdf = true;
            return this;
        }

        /**
         * No idf transformation count vectorizer builder.
         *
         * @return the count vectorizer builder
         */
        public CountVectorizerBuilder noIdfTransformation() {
            this.applyIdf = false;
            return this;
        }

        /**
         * With sublinear tf transformation count vectorizer builder.
         *
         * @return the count vectorizer builder
         */
        public CountVectorizerBuilder withSublinearTfTransformation() {
            this.sublinearTf = true;
            return this;
        }

        /**
         * With l 2 normalization count vectorizer builder.
         *
         * @return the count vectorizer builder
         */
        public CountVectorizerBuilder withL2Normalization() {
            this.normalize = true;
            return this;
        }

        /**
         * No l 2 normalization count vectorizer builder.
         *
         * @return the count vectorizer builder
         */
        public CountVectorizerBuilder noL2Normalization() {
            this.normalize = false;
            return this;
        }

        /**
         * Build count vectorizer.
         *
         * @return the count vectorizer
         */
        public CountVectorizer build() {
            return new CountVectorizer(minDf, applyIdf, sublinearTf, normalize);
        }

        /**
         * Fit count vectorizer.
         *
         * @param documents the documents
         * @return the count vectorizer
         */
        public CountVectorizer fit(List<List<String>> documents) {
            CountVectorizer vectorizer = build();
            vectorizer.fit(documents);
            return vectorizer;
        }

    }

    /**
     * Create count vectorizer builder.
     *
     * @return the count vectorizer builder
     */
    public static CountVectorizerBuilder create() {
        return new CountVectorizerBuilder();
    }

    /**
     * With default settings count vectorizer.
     *
     * @return the count vectorizer
     */
    public static CountVectorizer withDefaultSettings() {
        int minDf = 5;
        boolean applyIdf = true;
        boolean sublinearTf = true;
        boolean normalize = true;
        return new CountVectorizer(minDf, applyIdf, sublinearTf, normalize);
    }

    private final int minDf;
    private final boolean applyIdf;
    private final boolean sublinearTf;
    private final boolean normalize;

    private Map<String, Integer> tokenToIndex;
    private List<String> vocabulary;
    private double[] idfs;

    /**
     * Instantiates a new Count vectorizer.
     *
     * @param minDf       the min df
     * @param applyIdf    the apply idf
     * @param sublinearTf the sublinear tf
     * @param normalize   the normalize
     */
    public CountVectorizer(int minDf, boolean applyIdf, boolean sublinearTf, boolean normalize) {
        this.minDf = minDf;
        this.applyIdf = applyIdf;
        this.sublinearTf = sublinearTf;
        this.normalize = normalize;
    }

    /**
     * Fit transform sparse dataset.
     *
     * @param documents the documents
     * @return the sparse dataset
     */
    public SparseDataset fitTransform(List<List<String>> documents) {
        return fit(documents).transform(documents);
    }

    /**
     * Fit count vectorizer.
     *
     * @param documents the documents
     * @return the count vectorizer
     */
    public CountVectorizer fit(List<List<String>> documents) {
        Multiset<String> df = HashMultiset.create();
        documents.forEach(list -> df.addAll(Sets.newHashSet(list)));
        Multiset<String> docFrequency = Multisets.filter(df, p -> df.count(p) >= minDf);

        vocabulary = Ordering.natural().sortedCopy(docFrequency.elementSet());
        tokenToIndex = new HashMap<>(vocabulary.size());
        for (int i = 0; i < vocabulary.size(); i++) {
            tokenToIndex.put(vocabulary.get(i), i);
        }

        if (applyIdf) {
            idfs = calculateIdf(docFrequency, tokenToIndex, documents.size());
        }

        return this;
    }

    private static double[] calculateIdf(Multiset<String> domainFrequency, Map<String, Integer> tokenToIndex,
                                         int numDocuments) {
        double numDocumentsLog = Math.log(numDocuments + 1);

        double[] result = new double[tokenToIndex.size()];

        for (Entry<String> e : domainFrequency.entrySet()) {
            String token = e.getElement();
            double idf = numDocumentsLog - Math.log(e.getCount() + 1);
            result[tokenToIndex.get(token)] = idf;
        }

        return result;
    }

    /**
     * Transform sparse dataset.
     *
     * @param documents the documents
     * @return the sparse dataset
     */
    public SparseDataset transform(List<List<String>> documents) {
        int nrow = documents.size();
        int ncol = tokenToIndex.size();

        SparseDataset tfidf = new SparseDataset(ncol);

        for (int rowNo = 0; rowNo < nrow; rowNo++) {
            tfidf.set(rowNo, 0, 0.0);

            Multiset<String> row = HashMultiset.create(documents.get(rowNo));

            for (Entry<String> e : row.entrySet()) {
                String token = e.getElement();
                double tf = e.getCount();
                if (sublinearTf) {
                    tf = 1 + Math.log(tf);
                }

                if (!tokenToIndex.containsKey(token)) {
                    continue;
                }

                int colNo = tokenToIndex.get(token);

                if (applyIdf) {
                    double idf = idfs[colNo];
                    tfidf.set(rowNo, colNo, tf * idf);
                } else {
                    tfidf.set(rowNo, colNo, tf);
                }
            }
        }

        if (normalize) {
            tfidf.unitize();
        }

        return tfidf;
    }


    /**
     * Vocabulary list.
     *
     * @return the list
     */
    public List<String> vocabulary() {
        return vocabulary;
    }

}
