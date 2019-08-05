package de.viadee.xai.anchor.adapter.remotemodule;

import org.junit.jupiter.api.Test;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.InetAddress;
import java.net.Socket;

import static org.junit.jupiter.api.Assertions.assertTrue;

class AnchorServerTest {

    @Test
    public void testConnection() throws IOException, InterruptedException {
        final Thread t1 = new Thread(() -> {
            try {
                AnchorServer.main(new String[]{"-p", "6666"});
            } catch (IOException e) {
                e.printStackTrace();
            }
        });
        t1.start();

        //Thread.currentThread().sleep(1000);

        Socket clientSocket = new Socket(InetAddress.getByName(null), 6666);
        clientSocket.setSoTimeout(3000);
        PrintWriter out = new PrintWriter(clientSocket.getOutputStream(), true);
        BufferedReader in = new BufferedReader(new InputStreamReader(clientSocket.getInputStream()));

        out.println("{\"id\":\"d6740976-b535-11e9-89dd-8fc02ee8f6b9\",\"instance\":15}");
        String resp = in.readLine();
        assertTrue(resp.startsWith("{\"samplesToEvaluate\""));
    }
}