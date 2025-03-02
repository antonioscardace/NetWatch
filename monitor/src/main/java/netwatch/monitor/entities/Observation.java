package netwatch.monitor.entities;

import java.io.Serializable;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

// This class represents the "observes" entity in the database.
// The Primary Key is the composite field "observationId" defined by the Embedded class {@link ObservationId}.
// It represents the Many-To-Many association table between "utilities" and "contacts" tables.
// @author Antonio Scardace
// @version 1.0

@NoArgsConstructor
@AllArgsConstructor
@Data
public class Observation implements Serializable {

    private ObservationId observationId;
    
    private Utility utility;
    private Contact contact;
    private String environment;
}