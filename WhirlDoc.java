import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;

/**
 * WhirlDoc v1.0 by Víctor Ortiz (victormanuelortiz@gmail.com)
 * Date: July 5, 2005.
 *
 *     WhirlDoc is a tool for generating documentation from a plain Whirl 
 * source code.  This program divide a whirl code into separate instructions 
 * and describes every step of the execution based on the language definition.
 * See http://www.bigzaphod.org/whirl/
 *
 * This code is based on the Whirl programming language implementation by 
 * Coljac (http://coljac.net) on July 10, 2004. 
 * See http://www.bigzaphod.org/whirl/Whirl.java
 *
 * Use Java 1.5 to compile.
 * To compile: javac WhirlDoc.java
 * To use:     java WhirlDoc source.wrl
 *
 * Note: This code belongs to the public domain.
 */

public class WhirlDoc {

  private int c; // For IO

  private int[] memory = new int[1000];


  private static final int OPS = 0;
  private static final int MATH = 1;

  private int ring = OPS;
  private boolean[] clockWise = new boolean[]{true, true};
  private int[] index = new int[]{0, 0};
  private int[] value = new int[]{0, 0};

  private int progIndex = 0;
  private int memIndex = 0;
  private String program = "";
  private String[] row = new String [4];
  private int lastInstruction = -1;

  private InputStream stdin;

  private String[][] opsNames = new String[][]{
    {"Noop", "Exit", "One", "Zero", "Load", "Store", "PAdd", "DAdd", "Logic", "If", "IntIO", "AscIO"},
    {"Noop", "Load", "Store", "Add", "Mult", "Div", "Zero", "<", ">", "=", "Not", "Neg"}};

  public WhirlDoc(String fileName) {
    try {
      BufferedReader br = new BufferedReader(new FileReader(fileName));
      String line = "";
      StringBuffer prog = new StringBuffer();
      while ((line = br.readLine()) != null) {
        if (!line.startsWith("//")) {
          line = line.replaceAll("\\s", "");
          line = line.replaceAll("//.*", "");
          prog.append(line.trim());
        }
      }
      br.close();
      program = prog.toString();
    } catch (IOException e) {
      System.err.println("Can't read source file: " + e.getMessage());
      System.exit(1);
    }
    stdin = System.in;
  }

  public void run() {
    row[0] = "Code";
    row[1] = "Instruction";
    row[2] = "Effect (Last Value)";
    row[3] = "Direction";
    printRow ();
    clearRow ();
    System.out.println("---------- --------------- --------------------------------------------- ---------");
    while (progIndex < program.length()) {
      execute(Integer.parseInt("" + program.charAt(progIndex)));
    }
    System.out.println("");
  }

  private void execute(int instruction) {
    row[0] += String.valueOf(program.charAt(progIndex));
    progIndex++;
    switch (instruction) {
      case 1:
        index[ring] += (clockWise[ring] ? 1 : -1);
        if (index[ring] < 0) {
          index[ring] = opsNames[ring].length - 1;
        } else if (index[ring] > opsNames[ring].length - 1) {
          index[ring] = 0;
        }
        lastInstruction = 1;
        break;
      case 0:
        // Reverse
        clockWise[ring] = !clockWise[ring];
        if (lastInstruction == 0) {
          if (ring == OPS) {
            ops(index[OPS]);
            row[3] = "OPS ::";
          } else {
            math(index[MATH]);
            row[3] = "MATH::";
          }
          if (clockWise[ring]) {
            row[3] += "CW";
          } else {
            row[3] += "CCW";
          }
          printRow();
          clearRow();
          // Swap rings
          if (ring == OPS) {
            ring = MATH;
          } else {
            ring = OPS;
          }
          lastInstruction = -1;
        } else {
          lastInstruction = 0;
        }
        break;
    }
  }

  private void ops(int operation) {
    switch (operation) {
      case 0: // noop
        row[1]="OPS ::Noop";
        row[2]="Has no effect.";
        break;
      case 1: // Exit
        row[1]="OPS ::Exit";
        row[2]="Terminates the program.";
        printRow();
        System.exit(0);
        break;
      case 2: // One
        row[1]="OPS ::One";
        row[2]="Sets Value to 1 ("+value[OPS]+")";
        value[OPS] = 1;
        break;
      case 3: // Zero
        row[1]="OPS ::Zero";
        row[2]="Sets Value to 0 ("+value[OPS]+")";
        value[OPS] = 0;
        break;
      case 4: // Load
        row[1]="OPS::Load";
        row[2]="Sets Value to "+memory[memIndex]+" ("+value[OPS]+")";
        value[OPS] = memory[memIndex];
        break;
      case 5: // Store
        row[1]="OPS ::Store";
        row[2]="Sets Memory["+memIndex+"] to "+value[OPS]+" ("+memory[memIndex]+")";
        memory[memIndex] = value[OPS];
        break;
      case 6: // PAdd
        row[1]="OPS ::PAdd";
        row[2]="Sets program pointer to "+(progIndex+value[OPS]-1)+" ("+progIndex+")";
        break;
      case 7: // DAdd
        row[1]="OPS ::DAdd";
        row[2]="Sets memory pointer to "+(memIndex+value[OPS])+" ("+memIndex+")";
        memIndex += value[OPS];
        while (memIndex > memory.length - 1) {
          addMemory();
        }
        break;
      case 8: // Logic
        row[1]="OPS ::Logic";
        int val = value[OPS];
        if (memory[memIndex] == 0) {
          value[OPS] = 0;
        } else {
          value[OPS] = (value[OPS] == 0 ? 0 : 1);
        }
        row[2]="Memory["+memIndex+"] is "+memory[memIndex]+" and Value is set to "+value[OPS]+" ("+val+")";
        break;
      case 9: // if
        if (memory[memIndex] != 0) {
          row[2] = "Sets program pointer to "+(progIndex+value[OPS]-1)+" ("+progIndex+")";
        }
        else {
          row[2] = "Program pointer remains the same.";
        }
        row[1]="OPS ::If";
        break;
      case 10: // IntIO
        row[1]="OPS ::IntIO";
        if (value[OPS] != 0) {
          row[2]="Output = "+memory[memIndex];
        } else {
          row[2]="Input to Memory["+memIndex+"]";
        }
        break;
      case 11: // AscIO
        row[1]="OPS ::AscIO";
        if (value[OPS] != 0) {
          if (memory[memIndex] == 10) {
            row[2]="Output = LF ("+memory[memIndex]+")";
          }
          else {
            row[2]="Output = "+((char) memory[memIndex])+" ("+memory[memIndex]+")";
          }
        } else {
          row[2]="Input to Memory["+memIndex+"]";
        }
        break;
    }
  }

  private void math(int operation) {
    switch (operation) {
      case 0: // noop
        row[1]="MATH::Noop";
        row[2]="Has no effect.";
        break;
      case 1: // Load
        row[1]="MATH::Load";
        row[2]="Sets Value to "+memory[memIndex]+" ("+value[MATH]+")";
        value[MATH] = memory[memIndex];
        break;
      case 2: // Store
        row[1]="MATH::Store";
        row[2]="Sets Memory["+memIndex+"] to "+value[MATH]+" ("+memory[memIndex]+")";
        memory[memIndex] = value[MATH];
        break;
      case 3: // Add
        row[1]="MATH::Add";
        row[2]="Sets Value to "+(value[MATH] + memory[memIndex])+" ("+value[MATH]+")";
        value[MATH] += memory[memIndex];
        break;
      case 4: // Mult
        row[1]="MATH::Mult";
        row[2]="Sets Value to "+(value[MATH] * memory[memIndex])+" ("+value[MATH]+")";
         value[MATH] *= memory[memIndex];
        break;
      case 5: // Div
        row[1]="MATH::Div";
        row[2]="Sets Value to "+(value[MATH] / memory[memIndex])+" ("+value[MATH]+")";
        value[MATH] /= memory[memIndex];
        break;
      case 6: // Zero
        row[1]="MATH::Zero";
        row[2]="Sets Value to 0 ("+value[MATH]+")";
        value[MATH] = 0;
        break;
      case 7: // <
        row[1]="MATH::<";
        if(value[MATH] < memory[memIndex]) {
          row[2]="Value ("+value[MATH]+") is less than Memory["+memIndex+"] ("+memory[memIndex]+")";
          value[MATH] = 1;
        } else {
          row[2]="Value ("+value[MATH]+") is not less than Memory["+memIndex+"] ("+memory[memIndex]+")";
          value[MATH] = 0;
        }
        break;
      case 8: // >
        row[1]="MATH::>";
        if(value[MATH] > memory[memIndex]) {
          row[2]="Value ("+value[MATH]+") is greater than Memory["+memIndex+"] ("+memory[memIndex]+")";
          value[MATH] = 1;
        } else {
          row[2]="Value ("+value[MATH]+") is not greater than Memory["+memIndex+"] ("+memory[memIndex]+")";
          value[MATH] = 0;
        }
        break;
      case 9: // =
        row[1]="MATH::=";
        if(value[MATH] == memory[memIndex]) {
          row[2]="Value ("+value[MATH]+") is equal to Memory["+memIndex+"] ("+memory[memIndex]+")";
          value[MATH] = 1;
        } else {
          row[2]="Value ("+value[MATH]+") is equal to Memory["+memIndex+"] ("+memory[memIndex]+")";
          value[MATH] = 0;
        }
        break;
      case 10: // Not
        row[1]="MATH::Not";
        if(value[MATH]!=0) {
          row[2]="Value ("+value[MATH]+") is not 0";
          value[MATH] = 0;
        } else {
          row[2]="Value ("+value[MATH]+") is 0";
          value[MATH] = 1;
        }
        break;
      case 11: // Neg
        row[1]="MATH::Neg";
        row[2]="Sets Value to "+(value[MATH]*-1)+" * -1 ("+value[MATH]+")";
        value[MATH] *= -1;
        break;
    }
  }

  private void addMemory() {
    int[] newMemory = new int[memory.length + 1000];
    System.arraycopy(memory, 0, newMemory, 0, memory.length);
    memory = newMemory;
  }

  // IO Stuff
  private char readChar() {
    try {
      c = System.in.read();
      return (char) c;
    } catch (IOException e) {
      c = -1;
    }
    return (char) 0x0;
  }

  private int readInt() {
    StringBuffer s = new StringBuffer();

    // eat up whitespace
    while (!(c == -1) && Character.isWhitespace((char) c))
      readChar();

    // now get the string
    while (!(c == -1) && !Character.isWhitespace((char) c)) {
      s.append((char) c);
      readChar();
    }

    if (s.length() == 0)
      throw new RuntimeException("Tried to read from empty stdin");
    else
      return Integer.parseInt(s.toString().trim());
  }
  private void printRow () {
    System.out.format("%-10s %-15s %-45s %s\n", row[0], row[1], row[2], row[3]);
  }
  private void clearRow () {
    int i;
    for (i=0; i<row.length; i++) {
      row[i] = "";
    }
  }
  public static void main(String[] args) {
    if(args.length != 1) {
      System.out.println("Usage: WhirlDoc <input.wrl>\n");
      System.exit(0);
    }
    try {
      new WhirlDoc(args[0]).run();
    } catch (Exception e) {
      e.printStackTrace();
    }
  }
}
