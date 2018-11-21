package de.viadee.anchorj.perturbation.text;

import de.viadee.anchorj.text.TextInstance;
import de.viadee.anchorj.text.TextPerturbationFunction;
import opennlp.tools.postag.POSDictionary;
import opennlp.tools.postag.POSModel;
import opennlp.tools.postag.POSTaggerME;

import java.io.*;
import java.lang.reflect.Field;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * Implementation of a text perturbation function using OpenNLP to find a word's alternatives.
 * <p>
 * Caution: this is an example of an insufficient perturbation function.
 * The results returned are not of good quality.
 */
public class OpenNLPTextPerturbationFunction extends TextPerturbationFunction {

    /**
     * Instantiates the OpenNLP perturbator
     *
     * @param instance               the instance
     * @param tokenChangeProbability the token change probability
     */
    public OpenNLPTextPerturbationFunction(TextInstance instance, double tokenChangeProbability) {
        super(instance, tokenChangeProbability, createAlternatives(instance));
    }

    private static List<List<String>> createAlternatives(TextInstance instance) {
        try (InputStream posModelIn = readResource("en-pos-maxent.bin")) {

            final POSModel posModel = new POSModel(posModelIn);
            final POSTaggerME posTagger = new POSTaggerME(posModel);

            String[] tags = posTagger.tag(instance.getInstance());

            // "Hacky" way to get the internal dictionary to POS
            POSDictionary tagDictionary = (POSDictionary) posModel.getFactory().getTagDictionary();
            Field declaredField = POSDictionary.class.getDeclaredField("dictionary");
            declaredField.setAccessible(true);
            @SuppressWarnings("unchecked")
            Map<String, String[]> wordToTags = (Map<String, String[]>) declaredField.get(tagDictionary);
            return Stream.of(tags).map(tag ->
                    wordToTags.entrySet().stream().filter(e ->
                            Arrays.asList(e.getValue()).contains(tag))
                            .map(Map.Entry::getKey).collect(Collectors.toList()))
                    .collect(Collectors.toList());
        } catch (IOException | NoSuchFieldException | IllegalAccessException e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * Create input stream from file
     *
     * @param path the path
     * @return the input stream
     */
    private static InputStream readResource(final String path) {
        try {
            return new FileInputStream(new File(OpenNLPTextPerturbationFunction.class.getClassLoader()
                    .getResource(path).getFile()).getPath());
        } catch (FileNotFoundException e) {
            throw new RuntimeException(e);
        }
    }
}
