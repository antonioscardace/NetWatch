package netwatch.identitymanager.entities;

import jakarta.persistence.Column;
import jakarta.persistence.Embeddable;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

// It represents a composite Primary Key for the entity {@link Permission} of the table "permissions".
// It is composed by "role", "method" and "object" fields. The first one is a Foreign Key.
// @author Antonio Scardace
// @version 1.0

@NoArgsConstructor
@AllArgsConstructor
@Data
@Embeddable
public class PermissionId {

    @Column(name = "role", nullable = false)
    private String role;

    @Column(name = "method", nullable = false)
    private String method;

    @Column(name = "object", nullable = false)
    private String object;
}