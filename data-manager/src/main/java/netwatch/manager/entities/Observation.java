package netwatch.manager.entities;

import java.io.Serializable;

import jakarta.persistence.Column;
import jakarta.persistence.EmbeddedId;
import jakarta.persistence.Entity;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.Table;
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
@Entity
@Table(name = "observes")
public class Observation implements Serializable {

    @EmbeddedId
    private ObservationId observationId;

    @ManyToOne
    @JoinColumn(name = "address", referencedColumnName = "address", insertable = false, updatable = false)
    private Utility utility;

    @ManyToOne
    @JoinColumn(name = "contact", referencedColumnName = "value", insertable = false, updatable = false)
    private Contact contact;

    @Column(name = "environment", nullable = false)
    private String environment;
}