package de.viadee.xai.anchor.adapter.tabular.util;

import java.io.Serializable;
import java.util.stream.IntStream;
import java.util.stream.Stream;

/**
 * Provides some basic functions to handle arrays.
 */
public enum ArrayUtils {;

    /**
     * Appends a column to an existing table
     *
     * @param values the initial table
     * @param column the column to be appended
     * @return the new table
     */
    public static Serializable[][] appendColumn(Serializable[][] values, Serializable[] column) {
        Serializable[][] result = new Serializable[values.length][];
        for (int i = 0; i < result.length; i++) {
            Serializable[] subResult = new Serializable[values[i].length + 1];
            System.arraycopy(values[i], 0, subResult, 0, values[i].length);
            subResult[values[i].length] = column[i];
            result[i] = subResult;
        }
        return result;
    }

    /**
     * Reads all values from a specific table column
     *
     * @param values the table
     * @param column index of the column to be read
     * @return the column extracted
     */
    public static Serializable[] extractColumn(Serializable[][] values, int column) {
        Serializable[] result = new Serializable[values.length];
        for (int i = 0; i < values.length; i++) {
            result[i] = values[i][column];
        }
        return result;
    }

    /**
     * Reads all values from a specific table column
     *
     * @param values the table
     * @param column index of the column to be read
     * @return the column extracted
     */
    public static Integer[] extractIntegerColumn(Serializable[][] values, int column) {
        Integer[] result = new Integer[values.length];
        for (int i = 0; i < values.length; i++) {
            result[i] = (Integer) values[i][column];
        }
        return result;
    }

    /**
     * Removes a specific column index from a table
     *
     * @param values the table
     * @param column the column index to be removed
     * @return the table without the removed column
     */
    public static Serializable[][] removeColumn(Serializable[][] values, int column) {
        Serializable[][] result = new Serializable[values.length][];
        for (int i = 0; i < result.length; i++) {
            Serializable[] subResult = new Serializable[values[i].length - 1];
            int currentIndex = 0;
            for (int j = 0; j < values[i].length; j++) {
                if (j != column) {
                    subResult[currentIndex] = values[i][j];
                    currentIndex++;
                }
            }
            result[i] = subResult;
        }
        return result;
    }

    /**
     * Removes a specific column index from an integer table
     *
     * @param values the integer table
     * @param column the column index to be removed
     * @return the table without the removed column
     */
    public static Integer[][] removeIntegerColumn(Integer[][] values, int column) {
        Integer[][] result = new Integer[values.length][];
        for (int i = 0; i < result.length; i++) {
            Integer[] subResult = new Integer[values[i].length - 1];
            int currentIndex = 0;
            for (int j = 0; j < values[i].length; j++) {
                if (j != column) {
                    subResult[currentIndex] = values[i][j];
                    currentIndex++;
                }
            }
            result[i] = subResult;
        }
        return result;
    }


    /**
     * Tries to convert an object table containing integer values only to an integer table
     *
     * @param values the object table
     * @return the {@link Object} table
     */
    public static Serializable[][] transformToIntArray(Serializable[][] values) {
        Serializable[][] result = new Serializable[values.length][];
        for (int i = 0; i < values.length; i++) {
            result[i] = transformToIntArray(values[i]);
        }
        return result;
    }

    public static Integer[] transformToIntArray(Serializable[] value) {
        Integer[] intRow = new Integer[value.length];
        for (int j = 0; j < value.length; j++) {
            Serializable cell = value[j];
            if (cell == null) {
                intRow[j] = null;
            } else if (cell instanceof Integer)
                intRow[j] = (Integer) cell;
            else {
                //noinspection CaughtExceptionImmediatelyRethrown
                try {
                    intRow[j] = Integer.valueOf((String) cell);
                } catch (Exception e) {
                    throw e;
                }
            }
        }
        return intRow;
    }

    /**
     * Unboxes an array
     *
     * @param array the array
     * @return the "unboxed" array
     */
    public static int[] toPrimitiveArray(Integer[] array) {
        return Stream.of(array).mapToInt(i -> i).toArray();
    }

    /**
     * Unboxes an array
     *
     * @param array the array
     * @return the "unboxed" array
     */
    public static double[] toPrimitiveArray(Double[] array) {
        return Stream.of(array).mapToDouble(i -> i).toArray();
    }

    /**
     * Boxes an array
     *
     * @param array the array
     * @return the "boxed" array
     */
    public static Integer[] toBoxedArray(int[] array) {
        return IntStream.of(array).boxed().toArray(Integer[]::new);
    }

    /**
     * Boxes an array
     *
     * @param array the array
     * @return the "boxed" array
     */
    public static Double[][] toBoxedArray(double[][] array) {
        Double[][] result = new Double[array.length][];
        for (int i = 0; i < array.length; i++) {
            result[i] = new Double[array[i].length];
            for (int j = 0; j < array[i].length; j++)
                result[i][j] = array[i][j];
        }
        return result;
    }

    /**
     * Overwrites an existing column in an object table
     *
     * @param values        the table in which values shall be replaced
     * @param replaceValues the values to be placed
     * @param targetColumn  the column to be overwritten
     */
    public static void replaceColumnValues(Serializable[][] values, int[] replaceValues, int targetColumn) {
        for (int i = 0; i < values.length; i++) {
            values[i][targetColumn] = replaceValues[i];
        }
    }
}
