package de.viadee.xai.anchor.adapter.rmodule;

import de.viadee.xai.anchor.algorithm.AnchorConstructionBuilder;
import de.viadee.xai.anchor.algorithm.AnchorResult;
import de.viadee.xai.anchor.algorithm.coverage.DisabledCoverageIdentification;
import org.json.JSONObject;
import org.apache.commons.cli.*;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public class AnchorServer {

    public static void main(String[] args) throws Exception {

        Options options = new Options();

        Option port = new Option("p", "port", true, "port for socket connection");
        port.setRequired(true);
        options.addOption(port);

        CommandLineParser parser = new DefaultParser();
        HelpFormatter formatter = new HelpFormatter();
        CommandLine cmd = null;

        try {
            cmd = parser.parse(options, args);
        } catch (ParseException e) {
            System.out.println(e.getMessage());
            formatter.printHelp("utility-name", options);

            System.exit(1);
        }

        int portNr = Integer.parseInt(cmd.getOptionValue("port"));

        try (ServerSocket listener = new ServerSocket(portNr)) {
            System.out.println("Anchor server is running...");
            ExecutorService pool = Executors.newFixedThreadPool(4);
            while (true) {
                pool.execute(new AnchorService(listener.accept()));
            }
        }
    }

    private static class AnchorService implements Runnable {
        private Socket socket;

        AnchorService(Socket socket) {
            this.socket = socket;
        }

        @Override
        public void run() {
            System.out.println("Connected: " + socket);
            String input, output;
            boolean quit = false;
            RInstance instance;
            try {
                PrintWriter out = new PrintWriter(socket.getOutputStream(), true);
                BufferedReader in = new BufferedReader(new InputStreamReader(socket.getInputStream()));

                while (true) {
                    if ((input = in.readLine()) != null) {
                        try {
                            JSONObject obj = new JSONObject(input);

                            if (obj.has("quit") && obj.getInt("quit") == 1) {
                                quit = true;
                                return;
                            }

                            String id = obj.getString("id");
                            int count = obj.getInt("count");
                            String status = obj.getString("status");
                            int dimension = obj.getInt("instance");
                            // instance
                            instance = new RInstance(dimension);


                        AnchorResult anchorResult = new AnchorConstructionBuilder<>(
                                new RCallbackSamplingFunction(out, in),
                                instance,
                                0
                        )
                                .setCoverageIdentification(new DisabledCoverageIdentification())
                                .setTau(0.8)
                                .setTauDiscrepancy(0.1)
                                .build().constructAnchor();
                        // TODO set actual coverage ident fun

                            //send response
                            JSONObject response = new JSONObject();
                            response.append("id", id);
                            response.append("count", count + 1);
                            response.append("status", "response");
                            JSONObject anchorResultjson = new JSONObject(anchorResult);
                            response.append("anchorResult", anchorResultjson);




                            out.println(response.toString());
                        } catch (Exception e){
                            e.printStackTrace();
                        }

                    }
                }
            } catch (Exception e) {
                System.out.println("Error:" + socket);
            } finally {
                try {
                    socket.close();
                } catch (IOException e) {
                    e.printStackTrace();
                }
                System.out.println("Closed: " + socket);
                if (quit) System.exit(1);
            }
        }

    }

}
