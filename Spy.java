import java.util.*;
import java.util.Scanner;

public class Spy implements FieldBase{

    private String status = "Alive";
    private int spyID;

    //Spy constructor with spyID to be set at the time of instantiation.
    public Spy(int id){
        this.spyID = id;
    }

    //Calls the unregisterDead() method of FieldBase if the status of the Spy is dead.
    public void isDead(){
        if (status == "Dead") {
            FieldBase.unregisterDead(spyID);
        }
    }

    //Public methods to access Spy Status and Spy ID
    public String getStatus(){
        return status;
    }

    public int getSpyID(){
        return spyID;
    }

    //If a spy dies, its status can be changed to dead.
    public String setStatus(String s){
        status = s;
        return status;
    }

}