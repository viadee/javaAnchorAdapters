package de.viadee.xai.anchor.adapter.tabular.builder;

import de.viadee.xai.anchor.adapter.tabular.column.GenericColumn;
import de.viadee.xai.anchor.adapter.tabular.column.IgnoredColumn;
import de.viadee.xai.anchor.adapter.tabular.column.StringColumn;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Collection;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Consumer;

import static org.junit.jupiter.api.Assertions.*;

class AbstractTabularBuilderTest {

    private InputStream getTestCSV() {
        return getClass().getClassLoader().getResourceAsStream("AnchorTabularTest.csv");
    }

    @Test
    public void testPostBuildHandler() throws IOException {
        TestBuilder builder = new TestBuilder();

        builder.addColumn(new IgnoredColumn("1"));
        builder.addColumn(new IgnoredColumn("2"));
        builder.addColumn(new IgnoredColumn("3"));

        // Test starts here
        final AtomicInteger timesListenerCalled = new AtomicInteger(0);
        final AtomicBoolean consumerCalled = new AtomicBoolean(false);
        builder.addColumn(new StringColumn("4") {
            @Override
            public Consumer<String[]> getPostBuildListener() {
                timesListenerCalled.getAndIncrement();
                return column -> {
                    if (consumerCalled.get())
                        fail("Only to be called once");
                    consumerCalled.getAndSet(true);

                    assertArrayEquals(new String[] {"thirdattr1", "thirdattr2"}, column);
                };
            }
        });
        builder.addColumn(new IgnoredColumn("5"));


        builder.build(getTestCSV(), true);

        // Once for != null check and once for actual notifying
        assertEquals(2, timesListenerCalled.get());
        assertTrue(consumerCalled.get());
    }

    private class TestBuilder extends AbstractTabularBuilder<TestBuilder> {
        final Collection<GenericColumn> result = new ArrayList<>();

        void addColumn(GenericColumn column) {
            result.add(column);
        }

        @Override
        Collection<GenericColumn> getColumnDescriptions(int fileColumnSize) {
            return result;
        }
    }
}