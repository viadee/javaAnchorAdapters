package de.goerke.tobias.anchorj.perturbation.text;

import de.viadee.anchorj.PerturbationFunction;
import de.goerke.tobias.anchorj.text.TextInstance;
import org.junit.jupiter.api.Test;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;

class OpenNLPTextPerturbationFunctionTest {

    @Test
    void perturbSentence() {
        final int nrPerturbations = 100;

        PerturbationFunction.PerturbationResult<TextInstance> perturbationResult = new OpenNLPTextPerturbationFunction(new TextInstance("This is a good book ."), 0.5)
                .perturb(Collections.emptySet(), nrPerturbations);

        assertEquals(perturbationResult.getRawResult().length, nrPerturbations);
        assertEquals(perturbationResult.getFeatureChanged().length, nrPerturbations);
        //assertEquals(perturbationResult.featureCount, 6);

        perturbationResult = new OpenNLPTextPerturbationFunction(new TextInstance("This is a good book ."), 0.5)
                .perturb(new HashSet<>(Arrays.asList(3)), nrPerturbations);
        assertEquals(Stream.of(perturbationResult.getRawResult()).flatMap(Stream::of).filter(r -> Stream.of(r.getInstance()).anyMatch(token -> token.equals("good"))).count(),
                perturbationResult.getRawResult().length);
    }
}