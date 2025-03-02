package netwatch.manager.entities;

import java.io.Serializable;

import jakarta.persistence.Column;
import jakarta.persistence.Embeddable;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

// It represents a composite Primary Key for the entity {@link Observation} of the table "observes".
// It is composed by "address" and "contact" fields. They are both Foreign Keys.
// Respectively, they are the utility address and its referent contact.
// @author Antonio Scardace
// @version 1.0

@NoArgsConstructor
@AllArgsConstructor
@Data
@Embeddable
public class ObservationId implements Serializable {

    @Column(name = "address", nullable = false)
    private String address;

    @Column(name = "contact", nullable = false)
    private String contact;
}