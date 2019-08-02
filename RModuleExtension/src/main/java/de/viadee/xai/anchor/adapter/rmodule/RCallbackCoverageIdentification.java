package de.viadee.xai.anchor.adapter.rmodule;

import de.viadee.xai.anchor.algorithm.coverage.CoverageIdentification;

import java.io.BufferedReader;
import java.io.PrintWriter;
import java.io.IOException;
import java.util.Set;
import java.util.stream.Collectors;

import org.json.JSONObject;

public class RCallbackCoverageIdentification implements CoverageIdentification {

    private PrintWriter out;
    private BufferedReader in;

    public RCallbackCoverageIdentification(PrintWriter out, BufferedReader in){
        super();
        this.out = out;
        this.in = in;
    }


    @Override
    public double calculateCoverage(Set<Integer> featureSet) {

        JSONObject coverage = new JSONObject();
        coverage.append("status", "coverage_request");
        coverage.append("features", featureSet.stream().map(i -> i + 1).collect(Collectors.toSet()));
        out.println(coverage.toString());

        String input;
        double calculatedCoverage = 0;
        try {
            if ((input = in.readLine()) != null) {
                JSONObject obj = new JSONObject(input);
                calculatedCoverage = obj.getDouble("coverage");
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
        return calculatedCoverage;
    }
}
