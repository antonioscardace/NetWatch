package netwatch.monitor.entities;

import java.io.Serializable;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

// This class represents the "utilities" entity in the database.
// The Primary Key is the field "address".
// @author Antonio Scardace
// @version 1.0

@NoArgsConstructor
@AllArgsConstructor
@Data
public class Utility implements Serializable {

    private String address;
    private String name;
    private String type;
}