package netwatch.monitor.entities;

import java.io.Serializable;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

// This class represents the "contacts" entity in the database.
// The Primary Key is the field "value".
// @author Antonio Scardace
// @version 1.0

@NoArgsConstructor
@AllArgsConstructor
@Data
public class Contact implements Serializable {

    private String value;
    private String type;
}